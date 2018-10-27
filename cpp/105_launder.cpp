#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>
#include <new>
#include <new>
#include <cstddef>
#include <cassert>


int main(int argc, char **argv)
{
/* https://miyuki.github.io/2016/10/21/std-launder.html
   http://code.i-harness.com/ja/docs/cpp/utility/launder
 */

    struct X {
	const int n; // note: X has a const member
	int m;
    };
    {
	X *p = new X{3};
	const int a = p->n;
	new (p) X{5};       // p does not point to new object because X::n is const
	const int b = p->n; // undefined behavior, aliasing with previouse p->n

	const int x = p->m; // undefined behavior (even though m is non-const, p can't be used)
	const int c = std::launder(p)->n; // OK, std::launder(p) points to new object
	/* previouse aliasing with previouse p->n is discarded by compiler */
    }

    return 0;
}
