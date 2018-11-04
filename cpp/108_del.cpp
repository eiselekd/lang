#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>
#include <new>
#include <new>
#include <cstddef>
#include <cassert>
#include <type_traits>
#include <vector>
#include <functional>

struct a {
    virtual ~a() {
	printf("~a\n");
    }
};

struct b : a {
    virtual ~b() {
	printf("~b\n");
    }
};

int main(int argc, char **argv) {
    {
	//b b0;
    }
    {
	char *b0 = (char*)malloc(100);
	new(static_cast<void*>(b0)) b();

	a *a0 = reinterpret_cast<b*>(b0);
	//delete(a0); /* will trigger valgrind outputm, need free*/

	a0->~a();
	free(a0);

    }
    return 0;
}
