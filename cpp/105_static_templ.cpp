#include <iostream>
#include <new>
#include "key2type.h"

template <typename Key>
class m {
public:
    m() = delete;
    template <typename id>
    static int get(id i)
    {
	using u = decltype(key2type(i));
        auto& f = init_flag<u>;
	return f;
    }
    template <typename v>
    static bool init_flag;
};

/* instantiate static
   template member inside template */
template <typename Key>
template <typename v>
bool m<Key>::init_flag = false;

int main(int argc, char **argv)
{

    std::cout << m<int>::get(ID(1));
    m<int>::get(ID(2));

    return 0;
}
