/* http://eli.thegreenplace.net/2014/variadic-templates-in-c/#id8 */

#include <stdio.h>
#include <iostream>
#include <functional>

#include <iostream>
#include <vector>
#include <deque>
#include <list>
using namespace std;

template<typename T>
bool pair_comparer(T a) {
  std::cout << __PRETTY_FUNCTION__ << '\n';
  return false;
}
template<typename T>
bool pair_comparer(T a, T b) {
  std::cout << __PRETTY_FUNCTION__ << '\n';
  return a == b;
}
template<typename T, typename... Args>
bool pair_comparer(T a, T b, Args... args) {
  std::cout << __PRETTY_FUNCTION__ << '\n';
  return a == b && pair_comparer(args...);
}

int main()
{
  cout << pair_comparer(1.5, 1.5, 2, 2, 6, 6) << endl;
  cout << pair_comparer(1.5, 1.5, 2, 2, 6, 6, 7) << endl;
  return 0;
}
