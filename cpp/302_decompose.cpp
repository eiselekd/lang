// https://stackoverflow.com/questions/54188397/recreate-function-signature-and-call-via-template-packs-in-c

#include <iostream>
#include <string>
#include <array>
#include <vector>
#include <functional>

template <std::size_t...Is, typename ...Ts>
std::function<void(std::array<int, 5>)> addfunc(std::index_sequence<Is...>, void (*f)(Ts...)) {
    return [f](std::array<int, 5> const& a) { f(a[Is]...); };
}

template<typename ...Ts>
std::function<void(std::array<int, 5>)> addfunc(void (*f)(Ts...)) {
    return addfunc(std::index_sequence_for<Ts...>(), f);
}

int main()
{
    const std::array<int, 5> stack {{ 0,1,2,3,4 }};
    std::vector<std::function<void(std::array<int, 5>)>> ma;

    ma.push_back(addfunc(+[](int a) { std::cout << a << std::endl; }));
    ma.push_back(addfunc(+[](int a, int b) { std::cout << a << b << std::endl;}));
    ma.push_back(addfunc(+[](int a, int b, int c) { std::cout << a << b << c << std::endl; }));
    ma.push_back(addfunc(+[](int a, int b, int c, int d) { std::cout << a << b << c << d << std::endl; }));

    for (auto const& f : ma) {
        f(stack);
    }
}
