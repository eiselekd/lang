/* http://eli.thegreenplace.net/2014/perfect-forwarding-and-universal-references-in-c/ */
#include <cstdint>
#include <iostream>
#include <string>
#include <typeinfo>
#include <iostream>
#include <memory>
using namespace std;

int func(int a, int b) {
  return a+b;
};

template<class T>
T&& _forward(typename std::remove_reference<T>::type& t) noexcept {
  return static_cast<T&&>(t);
}

template <typename T1, typename T2>
int wrapper(T1&& e1, T2&& e2) {
  return func(_forward<T1>(e1), _forward<T2>(e2));
}

int main(int argc, char** argv) {
  cout << wrapper(42, 3.14f) << endl;
  return 0;
}
