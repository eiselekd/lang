libdir = File.dirname(__FILE__)
$LOAD_PATH.unshift("#{libdir}/metasm")

require "metasm"
include Metasm

exefmt = Metasm.const_get('ELF');
exe = exefmt.decode_file('f.exe')
dasm = exe.disassembler
d = exe.send(:disassemble)

dasm.dump(false);

di = dasm.each_instructionblock {
    |b|
    puts("----------------------");
    b.list.dup.each {
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
}
