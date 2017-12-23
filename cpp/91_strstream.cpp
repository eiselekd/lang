#include <string>
#include <iostream>
#include <sstream>

main() {
    std::stringstream ss;
    ss.width(10);
    ss << 100 << ' ' << 200;

    std::cout << ss.str() << std::endl;

    int foo,bar;
    ss >> foo >> bar;
    std::cout << "foo: " << foo << '\n';
    std::cout << "bar: " << bar << '\n';
    return 0;
}
