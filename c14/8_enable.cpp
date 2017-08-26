#include <cstdint>
#include <iostream>
#include <string>
#include <typeinfo>
#include <iostream>
#include <memory>
#include <type_traits>
using namespace std;


template <class T>
typename std::enable_if<std::is_integral<T>::value,int>::type
do_stuff(typename std::enable_if<std::is_integral<T>::value,int>::type &&x)
{
  return x;
}

/*template <typename T>
void do_stuff(std::enable_if_t<std::is_class<T>::value, T> &t) {}
*/

int main(int argc, char** argv) {
  //class a { public: a() {}; int b; };
  //a v;
  //do_stuff(1);
  do_stuff(1);
  return 0;
}
