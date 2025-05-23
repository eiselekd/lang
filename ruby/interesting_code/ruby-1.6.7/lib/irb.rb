#
#   irb.rb - irb main module
#   	$Release Version: 0.7.4 $
#   	$Revision: 1.1.2.3 $
#   	$Date: 2001/05/16 20:44:26 $
#   	by Keiju ISHITSUKA(keiju@ishitsuka.com)
#
# --
#
#
#
require "e2mmap"

require "irb/init"
require "irb/context"
require "irb/extend-command"
require "irb/workspace"

require "irb/ruby-lex"
require "irb/input-method"
require "irb/locale"

STDOUT.sync = true

module IRB
  @RCS_ID='-$Id: irb.rb,v 1.1.2.3 2001/05/16 20:44:26 keiju Exp $-'

  class Abort < Exception;end

  #
  @CONF = {}

  def IRB.conf
    @CONF
  end

  # IRB version method
  def IRB.version
    if v = @CONF[:VERSION] then return v end

    require "irb/version"
    rv = @RELEASE_VERSION.sub(/\.0/, "")
    @CONF[:VERSION] = format("irb %s(%s)", rv, @LAST_UPDATE_DATE)
  end

  # initialize IRB and start TOP_LEVEL irb
  def IRB.start(ap_path = nil)
    $0 = File::basename(ap_path, ".rb") if ap_path

    IRB.initialize(ap_path)
    IRB.parse_opts
    IRB.load_modules

    if @CONF[:SCRIPT]
      irb = Irb.new(nil, @CONF[:SCRIPT])
    else
      irb = Irb.new
    end

    @CONF[:IRB_RC].call(irb.context) if @CONF[:IRB_RC]
    @CONF[:MAIN_CONTEXT] = irb.context

    trap("SIGINT") do
      irb.signal_handle
    end
    
    catch(:IRB_EXIT) do
      irb.eval_input
    end
    print "\n"
  end

  def IRB.irb_exit(irb, ret)
    throw :IRB_EXIT, ret
  end

  def IRB.irb_abort(irb, exception = Abort)
    if defined? Thread
      irb.context.thread.raise exception, "abort then interrupt!!"
    else
      raise exception, "abort then interrupt!!"
    end
  end

  #
  # irb interpriter main routine 
  #
  class Irb
    def initialize(workspace = nil, input_method = nil)
      @context = Context.new(self, workspace, input_method)
      @context.main.extend ExtendCommand
      @signal_status = :IN_IRB

      @scanner = RubyLex.new
      @scanner.exception_on_syntax_error = false
    end
    attr_reader :context
    attr_accessor :scanner

    def eval_input
      @scanner.set_input(@context.io) do
	signal_status(:IN_INPUT) do
	  unless l = @context.io.gets
	    if @context.ignore_eof? and @context.io.readable_atfer_eof?
	      l = "\n"
	      if @context.verbose?
		printf "Use \"exit\" to leave %s\n", @context.ap_name
	      end
	    end
	  end
	  l
	end
      end

      @scanner.set_prompt do
	|ltype, indent, continue, line_no|
	if ltype
	  f = @context.prompt_s
	elsif continue
	  f = @context.prompt_c
	else @context.prompt_i
	  f = @context.prompt_i
	end
	f = "" unless f
	@context.io.prompt = p = prompt(f, ltype, indent, line_no)
	if @context.auto_indent_mode
	  unless ltype
	    ind = prompt(@context.prompt_i, ltype, indent, line_no).size + 
	      indent * 2 - p.size
	    ind += 2 if continue
	    @context.io.prompt = p + " " * ind if ind > 0
	  end
	end
      end
       
      @scanner.each_top_level_statement do
	|line, line_no|
	signal_status(:IN_EVAL) do
	  begin
	    trace_in do
	      @context._ = @context.workspace.evaluate(line, 
						       @context.irb_path, 
						       line_no)
#	      @context._ = irb_eval(line, @context.bind, @context.irb_path, line_no)
	    end

	    if @context.inspect?
	      printf @context.return_format, @context._.inspect
	    else
	      printf @context.return_format, @context._
	    end
	  rescue StandardError, ScriptError, Abort
	    $! = RuntimeError.new("unknown exception raised") unless $!
	    print $!.type, ": ", $!, "\n"
	    if  $@[0] =~ /irb(2)?(\/.*|-.*|\.rb)?:/ && $!.type.to_s !~ /^IRB/
	      irb_bug = true 
	    else
	      irb_bug = false
	    end
	    
	    messages = []
	    lasts = []
	    levels = 0
	    for m in $@
	      m = @context.workspace.filter_backtrace(m) unless irb_bug
	      if m
		if messages.size < @context.back_trace_limit
		  messages.push "\tfrom "+m
		else
		  lasts.push "\tfrom "+m
		  if lasts.size > @context.back_trace_limit
		    lasts.shift 
		    levels += 1
		  end
		end
	      end
	    end
	    print messages.join("\n"), "\n"
	    unless lasts.empty?
	      printf "... %d levels...\n", levels if levels > 0
	      print lasts.join("\n")
	    end
	    print "Maybe IRB bug!!\n" if irb_bug
	  end
	end
      end
    end

#     def irb_eval(line, bind, path, line_no)
#       id, str = catch(:IRB_TOPLEVEL_EVAL){
# 	return eval(line, bind, path, line_no)
#       }
#       case id
#       when :EVAL_TOPLEVEL
# 	eval(str, bind, "(irb_internal)", 1)
#       when :EVAL_CONTEXT
# 	@context.instance_eval(str)
#       else
# 	IRB.fail IllegalParameter
#       end
#     end

    def signal_handle
      unless @context.ignore_sigint?
	print "\nabort!!\n" if @context.verbose?
	exit
      end

      case @signal_status
      when :IN_INPUT
	print "^C\n"
	raise RubyLex::TerminateLineInput
      when :IN_EVAL
	IRB.irb_abort(self)
      when :IN_LOAD
	IRB.irb_abort(self, LoadAbort)
      when :IN_IRB
	# ignore
      else
	# ignore other cases as well
      end
    end

    def signal_status(status)
      return yield if @signal_status == :IN_LOAD

      signal_status_back = @signal_status
      @signal_status = status
      begin
	yield
      ensure
	@signal_status = signal_status_back
      end
    end

    def trace_in
      Tracer.on if @context.use_tracer?
      begin
	yield
      ensure
	Tracer.off if @context.use_tracer?
      end
    end

    def prompt(prompt, ltype, indent, line_no)
      p = prompt.dup
      p.gsub!(/%([0-9]+)?([a-zA-Z])/) do
	case $2
	when "N"
	  @context.irb_name
	when "m"
	  @context.main.to_s
	when "M"
	  @context.main.inspect
	when "l"
	  ltype
	when "i"
	  if $1 
	    format("%" + $1 + "d", indent)
	  else
	    indent.to_s
	  end
	when "n"
	  if $1 
	    format("%" + $1 + "d", line_no)
	  else
	    line_no.to_s
	  end
	when "%"
	  "%"
	end
      end
      p
    end

    def inspect
      ary = []
      for iv in instance_variables
	case iv
	when "@signal_status"
	  ary.push format("%s=:%s", iv, @signal_status.id2name)
	when "@context"
	  ary.push format("%s=%s", iv, eval(iv).__to_s__)
	else
	  ary.push format("%s=%s", iv, eval(iv))
	end
      end
      format("#<%s: %s>", type, ary.join(", "))
    end
  end

  # Singleton method
  def @CONF.inspect
    IRB.version unless self[:VERSION]

    array = []
    for k, v in sort{|a1, a2| a1[0].id2name <=> a2[0].id2name}
      case k
      when :MAIN_CONTEXT
	next
      when :PROMPT
	s = v.collect{
	  |kk, vv|
	  ss = vv.collect{|kkk, vvv| ":#{kkk.id2name}=>#{vvv.inspect}"}
	  format(":%s=>{%s}", kk.id2name, ss.join(", "))
	}
	array.push format("CONF[:%s]={%s}", k.id2name, s.join(", "))
      else
	array.push format("CONF[:%s]=%s", k.id2name, v.inspect)
      end
    end
    array.join("\n")
  end
end
