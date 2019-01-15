#include <iostream>
#include <functional>

template <typename T,typename = typename std::enable_if_t<std::is_integral<T>::value>>
void do_stuff(T&& t) {
    std::cout << "is integral\n";
}

template <typename T /* type of predicate do I need to use to match a lambda? */ >
void do_stuff(T&& t) {
    std::cout << "is lambda\n";
}

int main(int argc, char **argv) {
    int a = 10;
    do_stuff(a);

    /* what type of predicate do I need to use above? */
    do_stuff([](){});
    return 0;
}
