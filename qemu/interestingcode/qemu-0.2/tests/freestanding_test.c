#include <asm/unistd.h>

extern inline volatile void exit(int status)
{
  int __res;
  __asm__ volatile ("movl %%ecx,%%ebx\n"\
		    "int $0x80" \
		    :  "=a" (__res) : "0" (__NR_exit),"c" ((long)(status)));
}

extern inline int write(int fd, const char * buf, int len)
{
  int status;
  __asm__ volatile ("pushl %%ebx\n"\
		    "movl %%esi,%%ebx\n"\
		    "int $0x80\n" \
		    "popl %%ebx\n"\
		    : "=a" (status) \
		    : "0" (__NR_write),"S" ((long)(fd)),"c" ((long)(buf)),"d" ((long)(len)));
}

void print_str(const char *str)
{
    int len = 0;
    while (str[len]) {
        len++;
    }
    write(1, str, len);
}

void print_hex(unsigned int val)
{
    char buf[9];
    int i;
    for (i = 7; i >= 0; i--) {
        int nibble = val & 0xf;
        if (nibble < 10) {
            buf[i] = '0' + nibble;
        } else {
            buf[i] = 'a' + (nibble - 10);
        }
        val >>= 4;
    }
    buf[8] = '\0';
    print_str(buf);
}

void print_nl(void)
{
    print_str("\n");
}

void _start(void)
{
    print_str("Starting freestanding 386 test...\n");

    // Test 1: Simple Arithmetic
    unsigned int a = 0x12345678;
    unsigned int b = 0xabcdef01;
    unsigned int c = a + b;
    print_str("Addition (0x12345678 + 0xabcdef01): ");
    print_hex(c); // should be 0xbe024579
    print_nl();

    // Test 2: Multiplication and division
    unsigned int d = 12345 * 6789;
    print_str("Multiplication (12345 * 6789): ");
    print_hex(d); // 12345 * 6789 = 83810205 = 0x04ff22dd
    print_nl();

    unsigned int e = 0x12345678 / 100;
    print_str("Division (0x12345678 / 100): ");
    print_hex(e); // 0x12345678 / 100 = 305419896 / 100 = 3054198 = 0x002e9a76
    print_nl();

    // Test 3: Bitwise and shift operations
    unsigned int f = a << 4;
    print_str("Shift Left (0x12345678 << 4): ");
    print_hex(f); // 0x23456780
    print_nl();

    unsigned int g = b >> 8;
    print_str("Shift Right (0xabcdef01 >> 8): ");
    print_hex(g); // 0x00abcdef
    print_nl();

    // Test 4: Loops & Accumulation
    unsigned int sum = 0;
    int i;
    for (i = 1; i <= 100; i++) {
        sum += i;
    }
    print_str("Loop sum 1..100: ");
    print_hex(sum); // 5050 = 0x000013ba
    print_nl();

    print_str("Freestanding 386 test complete.\n");
    exit(0);
}
