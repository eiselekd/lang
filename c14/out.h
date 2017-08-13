/* https://stackoverflow.com/questions/213761/what-are-some-uses-of-template-template-parameters-in-c/14311714#14311714 */
#include <stdio.h>
#include <iostream>
#include <functional>

#include <iostream>
#include <vector>
#include <deque>
#include <list>

template<typename T, template<class,class...> class C, class... Args>
std::ostream& operator <<(std::ostream& os, const C<T,Args...>& objs)
{
    os << __PRETTY_FUNCTION__ << '\n';
    for (auto const& obj : objs)
        os << obj << ' ';
    return os;
}
