#ifndef _METHOD_C_HEADER_
#define _METHOD_C_HEADER_

#include <iostream>
#include <string>
#include <array>
#include <map>
#include <functional>
#include <type_traits>

#include "cid_header.h"
#include "method.h"

typedef method method_c_base;

template<class Ld, typename = void>
struct method_c : method_c<decltype(&Ld::operator())>
{
    typedef method_c<decltype(&Ld::operator())> _BASE;
    method_c(Ld &&f) : _BASE(std::forward<Ld>(f)) {};
};

template<class Ret, class Cls, class... ArgTypes>
struct method_c<Ret(Cls::*)(ArgTypes...)>
    : method_c_base
{
    Cls fn;
    method_c(Cls &&f) : fn(f) {
	std::cout << "[]()\n";
    }

    template <std::size_t...Is>
    inline void call_0(std::index_sequence<Is...>)
    {
	CID *stk;
	int argcnt = sizeof...(ArgTypes);
	fn((ArgTypes)stk[Is]...);
    }
    void call() { return call_0(std::index_sequence_for<ArgTypes...>()); }
};

template<class Ret, class Cls, class... ArgTypes>
struct method_c<Ret(Cls::*)(ArgTypes...) const>
    : method_c_base
{
    const Cls fn;
    method_c(const Cls &&f) : fn(f) {
	std::cout << "[]()\n";
    }

    template <std::size_t...Is>
    inline void call_0(std::index_sequence<Is...>)
    {
	CID *stk;
	int argcnt = sizeof...(ArgTypes);
	fn((ArgTypes)stk[Is]...);
    }
    void call() { return call_0(std::index_sequence_for<ArgTypes...>()); }
};

template <typename RetType, typename... ArgTypes>
struct method_c<std::function<RetType(ArgTypes...)>>
    : method_c_base
{
    typedef std::function<RetType(ArgTypes...)> FTYPE;
    FTYPE fn;
    method_c(FTYPE &&f) : fn(f) { };

    template <std::size_t...Is>
    inline void call_0(std::index_sequence<Is...>)
    {
	CID *stk;
	int argcnt = sizeof...(ArgTypes);
	fn((ArgTypes)stk[Is]...);
    }
    void call() { return call_0(std::index_sequence_for<ArgTypes...>()); }
};

template <typename RetType, typename... ArgTypes>
struct method_c<RetType(*)(ArgTypes...)>
    : method_c_base
{
    typedef RetType(*FTYPE)(ArgTypes...) ;
    FTYPE fn;
    method_c(RetType(*f)(ArgTypes...)) : fn(f) {
	std::cout << "*()\n";
    };

    template <std::size_t...Is>
    inline void call_0(std::index_sequence<Is...>)
    {
	CID *stk;
	int argcnt = sizeof...(ArgTypes);
	fn((ArgTypes)stk[Is]...);
    }
    void call() { return call_0(std::index_sequence_for<ArgTypes...>()); }
};

template <typename RetType, typename... ArgTypes>
struct method_c<RetType(ArgTypes...)> : method_c<RetType(*)(ArgTypes...)>
{
    method_c(RetType(&f)(ArgTypes...)) : method_c<RetType(*)(ArgTypes...)>(&f) {};
};

#ifdef _GEN_MAIN_

int
main(int argc, char **argv) {
    new method_c([](int a) {});
    new method_c<decltype(f1)>(f1);
    return 0;
}

#endif

#endif
