from __future__ import print_function
from unicorn import *
from unicorn.x86_const import *
from keystone.keystone import *

def convert_binary(a):
    b = b'';
    for i in a:
        b += '{0:08b}'.format(i)
    return b;

# separate assembly instructions by ; or \n
CODE = b'''
inc ecx
'''

# callback for tracing memory access (READ or WRITE)
def hook_mem_access(uc, access, address, size, value, user_data):
    if access == UC_MEM_WRITE:
        print(">>> Memory is being WRITE at 0x%x, data size = %u, data value = 0x%x" \
                %(address, size, value))
    else:   # READ
        print(">>> Memory is being READ at 0x%x, data size = %u" \
                %(address, size))


# Initialize engine in X86-32bit mode
ks = Ks(KS_ARCH_X86, KS_MODE_32)
encoding, count = ks.asm(CODE)
print("%s = %s (number of statements: %u)" %(CODE, encoding, count))

e = bytes(encoding)

# Initialize emulator in X86-32bit mode
mu = Uc(UC_ARCH_X86, UC_MODE_32)

# memory address where emulation starts
ADDRESS = 0x1000000

# map 2MB memory for this emulation
mu.mem_map(ADDRESS, 2 * 1024 * 1024)
mu.mem_write(ADDRESS, e)
mu.reg_write(UC_X86_REG_ECX, 0x1234)

# tracing all memory READ & WRITE access
mu.hook_add(UC_HOOK_MEM_WRITE, hook_mem_access)
mu.hook_add(UC_HOOK_MEM_READ, hook_mem_access)

# setup stack
mu.reg_write(UC_X86_REG_RSP, ADDRESS + 0x200000)

# emulate machine code in infinite time
mu.emu_start(ADDRESS, ADDRESS + len(e))

r_ecx = mu.reg_read(UC_X86_REG_ECX)
print(">>> ECX = 0x%x" %r_ecx)
