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

using integer = std::make_signed<std::size_t>::type;

template<typename CharT, typename Traits, typename... Values>
inline int printf( std::basic_ostream<CharT, Traits>& out, const CharT* format, Values&&... values) {
    std::tuple<Values&...> tuple(values...);

    integer nof_args = 0;
}

template<typename... Values>
inline std::string stream_snprintf(const char* format, Values&&... values) {
   std::ostringstream os;
   printf(os, format, std::forward<Values>(values)...);
   return os.str();
}

int main(int argc, char **argv) {
    std::string a = stream_snprintf("Test %d\n", 1);
    return 0;
}
