#include <iostream>
#include <string>
#include <array>
#include <map>
#include <functional>

void f1(int) {};

template<typename C>
void func(C &&a) {
    std::cout << "func 0" << std::endl;
}

int
main(int argc, char **argv) {

    func(f1);
    func([](){});
    return 0;
}
