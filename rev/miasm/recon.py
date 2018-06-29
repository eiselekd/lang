from miasm2.ir.symbexec import symbexec
from miasm2.analysis.machine import Machine
from miasm2.analysis.binary import Container
from miasm2.ir.translators import Translator
from miasm2.expression.expression import *
import z3

# Open the ELF binary
cont = Container.from_stream(open("add"))
machine = Machine(cont.arch)
# Get the address of add()
addr = cont.symbol_pool["add"].offset

# Disassemble the function and add blocs to the ira object
cfg = machine.dis_engine(cont.bin_stream).dis_multiblock(addr)
ira = machine.ira()
[ira.add_block(bloc) for bloc in cfg]
# Create the symbolic execution object
symb = symbexec(ira, ira.arch.regs.regs_init)
# Emulate using 0x800 for RDI
symb.symbols[ExprId("RDI", 64)] = ExprInt(0x800, 64)
symb.emul_ir_blocs(ira, addr)
# Get the return equation
ret = symb.symbols[ira.ret_reg]; print "Equation:", ret

# Convert miasm constraint to a z3 one
trans = Translator.to_language("z3")
constraint = ExprAff(ExprInt(0x2807, ret.size), ret)
# Solve using z3
solver = z3.Solver()
solver.add(trans.from_expr(constraint))
if solver.check() == z3.sat:
    model = solver.model()
    for expr in ret.get_r():
        print "Result:", expr, model.eval(trans.from_expr(expr))
