/* https://stackoverflow.com/questions/213761/what-are-some-uses-of-template-template-parameters-in-c/14311714#14311714 */
#include <stdio.h>
#include <iostream>
#include <functional>

#include <iostream>
#include <vector>
#include <deque>
#include <list>
using namespace std;

template<typename T, template<class,class...> class C, class... Args>
std::ostream& operator <<(std::ostream& os, const C<T,Args...>& objs)
{
    os << __PRETTY_FUNCTION__ << '\n';
    for (auto const& obj : objs)
        os << obj << ' ';
    return os;
}

int main()
{
  std::cout << __PRETTY_FUNCTION__ << "\n";
    std::vector<float> vf { 1.1, 2.2, 3.3, 4.4 };
    std::cout << vf << '\n';

    std::list<char> lc { 'a', 'b', 'c', 'd' };
    std::cout << lc << '\n';

    std::deque<int> di { 1, 2, 3, 4 };
    std::cout << di << '\n';

    return 0;
}
