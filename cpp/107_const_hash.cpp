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

using HashVal = int;

/* https://www.youtube.com/watch?v=dV0FTkEl0W4 */

constexpr HashVal operator "" _Hash(const char *str, std::size_t size) {
    int h = 0;
    for (auto i = 0UL; i < size; i++) {
	h += str[0] ^ (h << 8);
    }
    return h;
}

int main(int argc, char **argv) {
    assert("Test"_Hash == 1);
    return 0;
}
