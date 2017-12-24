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

template <typename... Args, std::size_t... Idx>
void apply(const std::tuple<Args...> &v, std::index_sequence<Idx...>) {
    // https://www.murrayc.com/permalink/2015/12/05/modern-c-variadic-template-parameters-and-tuples/
    // fold "," operator (c17), https://blog.tartanllama.xyz/exploding-tuples-fold-expressions/
    // http://en.cppreference.com/w/cpp/language/fold
    ((std::cout << std::get<Idx>(v) << "\n"),...);
    return;
}

template<typename CharT, typename Traits, typename... Args>
inline int printf( std::basic_ostream<CharT, Traits>& out, const std::tuple<Args...>& t) {

    apply(t, std::index_sequence_for<Args...>{});
    integer nof_args = 0;
}


int main(int argc, char **argv) {
    std::ostringstream os;
    printf(os, std::make_tuple(1, 42.1, 2));
    return 0;
}
