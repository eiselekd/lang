// https://stackoverflow.com/questions/54188397/recreate-function-signature-and-call-via-template-packs-in-c

#include <iostream>
#include <string>
#include <array>
#include <vector>
#include <functional>
#include <vector>

template<typename T> struct tag {};

template<typename TFunctionObject, typename TFirstParamType>
struct Binder {
    TFunctionObject function;
    TFirstParamType param;

    template<typename... TParams>
    auto operator()(TParams&&... params)
	-> decltype(function(param, std::forward<TParams>(params)...))
    {
	return function(param, std::forward<TParams>(params)...);
    }
};

template<typename TCallback>
static void readIntoFunction(int *stack, TCallback&& callback)
{
    callback();
}
template<typename TCallback, typename TFirstType, typename... TTypes>
static void readIntoFunction(int *stack, TCallback&& callback, tag<TFirstType>, tag<TTypes>... othersTags)
{
    Binder<TCallback, const TFirstType&> binder{ callback, *stack };
    return readIntoFunction(++stack, binder, othersTags...);
}

/* decompose arguments */
template<typename TFunctionType, typename... TOtherParams>
std::function<void(int*)> _addfunc(TFunctionType f, tag<void (*)(TOtherParams...)>) {
    return std::function<void(int*)>([f](int *stack) {
	    readIntoFunction(stack, f, tag<TOtherParams>{}...);
	});
}

template<typename TFunctionType>
std::function<void(int*)> addfunc(TFunctionType fn)
{
    return _addfunc(std::move(fn), tag<TFunctionType>{} );
}

void f1(int a0) { std::cout << a0 << std::endl; }
void f2(int a0, int a1) { std::cout << a0 << a1 << std::endl;  }

int main() {
    int stack[5] = { 0,1,2,3,4 };
    auto a0 = addfunc(&f1);
    auto a1 = addfunc(&f2);

    a0(stack);
    a1(stack);
}
