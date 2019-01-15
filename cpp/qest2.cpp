#include <iostream>
#include <string>
#include <array>
#include <map>
#include <functional>

template<typename T> struct tag {};
struct mybase {};

template<class Ld>
struct split : split<decltype(&Ld::operator())>
{
    split(Ld &&f) : split<decltype(&Ld::operator())>(std::forward<Ld>(f)) {};
};

template <typename RetType, typename... ArgTypes>
struct split<std::function<RetType(ArgTypes...)>>  {
    split(std::function<RetType(ArgTypes...)> &&f) {
	std::cout << "std::function" << std::endl;
    };
};

template <typename RetType, typename... ArgTypes>
struct split<RetType(*)(ArgTypes...)> {
    split(RetType(*f)(ArgTypes...)) {
	std::cout << "func-ptr" << std::endl;
    };
};

template <typename RetType, class Cls, typename... ArgTypes>
struct split<RetType(Cls::*)(ArgTypes...) const >  {
    split(const Cls &&f) {
	std::cout << "[]() const" << std::endl;
    };
};

template <typename RetType, class Cls, typename... ArgTypes>
struct split<RetType(Cls::*)(ArgTypes...) >  {
    split(Cls &&f) {
	std::cout << "[]()" << std::endl;
    };
};


void f1(int) {};

int
main(int argc, char **argv) {

    new split<std::decay<decltype(f1)>::type>(f1);
    new split<std::function<void(int)>>(std::function<void(int)>([](int) {}));

    /* no g++-17: */
    //auto l = [](int){};
    //new split<decltype(l)>(std::forward<decltype(l)>(l));

    /* g++-17: */
    new split([](int){});

    return 0;
}
