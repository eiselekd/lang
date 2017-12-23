#include <iostream>
#include <stdarg.h>

void print(const char *f, int l)
{
    printf("1:%.*s\n", l , f);
}

void print(const char *f, ...)
{
    va_list ap;
    va_start ( ap, f );
    vprintf(f, ap);
    va_end ( ap );
}



int main() {
    print("first", 2);
    print("second %d:%d\n", 2, 3);
}
