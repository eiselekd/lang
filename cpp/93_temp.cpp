#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <cassert>
#include <cerrno>
#include <climits>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <cwchar>
#include <cwctype>
#include <exception>
#include <iomanip>
#include <iostream>
#include <limits>
#include <locale>
#include <sstream>
#include <streambuf>
#include <string>
#include <tuple>
#include <type_traits>

// https://stackoverflow.com/questions/6245735/pretty-print-stdtuple/6245777#6245777

using integer = std::make_signed<std::size_t>::type;

template <typename Values>
void print(const char *fmt, Values&& v) {
    std::cout << fmt[0] << v << std::endl;
}

template <typename V, typename ...Values>
void print(const char *fmt, V&& x, Values&&...v) {
    std::cout << fmt[0] << x << std::endl;
    print(++fmt, v...);
}

int main(int argc, char **argv) {
    std::ostringstream os;
    print("abcdef", 2, 4, 6, "a");
    return 0;
}
