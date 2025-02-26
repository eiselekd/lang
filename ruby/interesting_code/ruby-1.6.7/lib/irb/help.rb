#
#   irb/help.rb - print usase module
#   	$Release Version: 0.7.3$
#   	$Revision: 1.1.2.1 $
#   	$Date: 2001/04/30 18:39:35 $
#   	by Keiju ISHITSUKA(keiju@ishitsuka.com)
#
# --
#
#   
#

module IRB
  def IRB.print_usage
    lc = IRB.conf[:LC_MESSAGES]
    path = lc.find("irb/help-message")
    space_line = false
    File.foreach(path) do
      |l|
      if /^\s*$/ =~ l
	lc.puts l unless space_line
	space_line = true
	next
      end
      space_line = false
      
      l.sub!(/#.*$/, "")
      next if /^\s*$/ =~ l
      lc.puts l
    end
  end
end

