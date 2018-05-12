#!/usr/bin/python
#http://www.miasm.re/blog/2017/10/05/playing_with_dynamic_symbolic_execution.html

from miasm2.analysis.sandbox import Sandbox_Linux_x86_64
from miasm2.jitter.csts import PAGE_READ
from miasm2.analysis.dse import DSEEngine
from miasm2.expression.expression import ExprInt
from miasm2.expression.expression import ExprId
from miasm2.analysis.dse import DSEPathConstraint
    

# Create sandbox
parser = Sandbox_Linux_x86_64.parser(description="ELF sandboxer")
parser.add_argument("filename", help="ELF Filename")
options = parser.parse_args()
options.jitter = "llvm"


# Force environment simulation
options.mimic_env = True
# Dummy argument: 123456789
options.command_line = ["".join(chr(0x30 + i) for i in xrange(1, 10))]


VALUE = ExprId("VALUE", 64)

def xxx_strtoul(jitter):
    global dse
    ret_ad, args = jitter.func_args_systemv(["nptr", "endptr", "base"])
    assert args.endptr == 0
    content = jitter.get_str_ansi(args.nptr)
    value = int(content, args.base)
    print "%r -> %d" % (content, value)
    #return
    jitter.func_ret_systemv(ret_ad, value)
    dse.attach(jitter)
    dse.update_state_from_concrete()
    dse.update_state({
        dse.ir_arch.arch.regs.RAX: VALUE,
    })
    
def xxx_printf_symb_(dse):
    result = dse.eval_expr(dse.ir_arch.arch.regs.RSI)
    print ("x" %(result))
    raise RuntimeError("Exit")

def xxx_printf_symb(dse):
    result = dse.eval_expr(dse.ir_arch.arch.regs.RSI)
    print result
    obtained = dse.symb.expr_simp(result.replace_expr({VALUE: ExprInt(123456789, 64)}))
    print obtained
    assert int(obtained) == sb.jitter.cpu.RSI
    raise RuntimeError("Exit")

# Instantiate and run
sb = Sandbox_Linux_x86_64(options.filename, options, globals())

# Init stack canary
sb.jitter.ir_arch.do_all_segm = True
FS_0_ADDR = 0x7ff70000
sb.jitter.cpu.FS = 0x4
sb.jitter.cpu.set_segm_base(sb.jitter.cpu.FS, FS_0_ADDR)
sb.jitter.vm.add_memory_page(
    FS_0_ADDR + 0x28, PAGE_READ, "\x42\x42\x42\x42\x42\x42\x42\x42", "Stack canary FS[0x28]")

#dse = DSEEngine(sb.machine)
dse = DSEPathConstraint(sb.machine)
dse.add_lib_handler(sb.libs, globals())

sb.run()

