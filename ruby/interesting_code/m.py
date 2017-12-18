#!/usr/bin/env ruby

libdir = File.dirname(__FILE__)
$LOAD_PATH.unshift("#{libdir}/metasm") 

require "metasm"
include Metasm
# produce x86 code
sc = Metasm::Shellcode.assemble(Metasm::Ia32.new, <<EOS)
add eax, 0x1234
mov [eax], 0x1234
ret
EOS

dasm = sc.init_disassembler
# disassemble handler code
dasm.disassemble(0)
# get decoded instruction at address 0
# then its basic block
bb = dasm.di_at(0).block
# display disassembled code
puts "\n[+] generated code:"
puts bb.list


# run though the basic block's list of decoded instruction
bb.list.each{
    |di|
    puts "\n[+] #{di.instruction}"
    sem = di.backtrace_binding()
    
    
    puts " data flow:"
    sem.each{|key, value| puts " #{key} => #{value}"}
    # does instruction modify the instruction pointer ?
    if di.opcode.props[:setip]
      puts " control flow:"
      # then display control flow semantics
      puts " * #{dasm.get_xrefs_x(di)}"
    end
    
}
    
