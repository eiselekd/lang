#include <iostream>
#include <string>
#include <array>
#include <map>
#include <functional>
#include <type_traits>

// namespace lambda_detail
// {
//     template<class Ret, class Cls, class IsMutable, class... Args>
//     struct types
//     {
// 	using is_mutable = IsMutable;

// 	enum { arity = sizeof...(Args) };

// 	using return_type = Ret;

// 	template<size_t i>
// 	struct arg
// 	{
// 	    typedef typename std::tuple_element<i, std::tuple<Args...>>::type type;
// 	};
//     };
// }

// template<class Ld>
// struct lambda_type : lambda_type<decltype(&Ld::operator())>
// {};

// template<class Ret, class Cls, class... Args>
// struct lambda_type<Ret(Cls::*)(Args...)>
//     : lambda_detail::types<Ret,Cls,std::true_type,Args...>
// {
//     lambda_type(Cls &&f) {
// 	printf("[]()\n");
//     }
// };

// template<class Ret, class Cls, class... Args>
// struct lambda_type<Ret(Cls::*)(Args...) const>
//     : lambda_detail::types<Ret,Cls,std::false_type,Args...>
// {
//     lambda_type(const Cls &&f) {
// 	printf("[]()\n");
//     }
// };



struct mybase {};

template<typename T> struct tag {};

template<class Ld>
struct method_c : method_c<decltype(&Ld::operator())>
{
    method_c(Ld &&f) : method_c<decltype(&Ld::operator())>(std::forward<Ld>(f)) {};
};

template<class Ret, class Cls, class... Args>
struct method_c<Ret(Cls::*)(Args...)>
    : mybase
{
    method_c(Cls &&f) {
	printf("[]()\n");
    }
};

template<class Ret, class Cls, class... Args>
struct method_c<Ret(Cls::*)(Args...) const>
    : mybase
{
    method_c(const Cls &&f) {
	printf("[]()\n");
    }
};


template<typename TData>
void add_c_method(int id, TData data) {
    typedef typename std::decay<TData>::type RealDataType;

    new method_c<TData>(std::forward<TData>(data));;
}

void f1() {};

int
main(int argc, char **argv) {
    //auto a = []() {};

    //add_c_method(1, f1 );
    //add_c_method(2, std::function<void()>([]() {}) );

    auto l = [](int a) {};

    //new method_c<decltype(l)>(std::forward<decltype(l)>(l));

    //add_c_method(2, [](int a) {});

    new method_c([](int a) {});
    return 0;
}
