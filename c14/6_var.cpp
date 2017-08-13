/* http://eli.thegreenplace.net/2014/variadic-templates-in-c/#id8 */
// To compile with dumping record layout:
// $ clang++ -o variadic-tuple -Xclang -fdump-record-layouts variadic-tuple.cpp
//           -Wall -g -std=c++11
#include <cstdint>
#include <iostream>
#include <string>
#include <typeinfo>
#include <iostream>
#include <memory>
using namespace std;

template<typename T, typename... Args>
unique_ptr<T> _make_unique(Args&&... args)
{
    std::cout << __PRETTY_FUNCTION__ << '\n';
    return unique_ptr<T>(new T(std::forward<Args>(args)...));
}

class FooType {
public: FooType(int a, std::string s, float b) {}
};

int main(int argc, char** argv) {

  std::unique_ptr<FooType> f = _make_unique<FooType>(1, "str", 2.13);
  return 0;
}
