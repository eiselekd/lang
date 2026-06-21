#include <inttypes.h>

uint32_t div64(uint32_t *q_ptr, uint64_t num, uint32_t den)
{
    *q_ptr = num / den;
    return num % den;
}

int32_t idiv64(int32_t *q_ptr, int64_t num, int32_t den)
{
    *q_ptr = num / den;
    return num % den;
}

void __attribute__((constructor, used)) dummy_init(void)
{
    asm volatile ("");
}
