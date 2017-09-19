#include <stdio.h>
#include <iostream>
#include <string>

class b {
public:
  b(const char *n) : n(n) {};
  operator const char *() const {
    return n;
  }
  const char *n;
};

class a {
public:
  a(const char *n) : n(n) {};
  operator b() const {
    return b(n);
  }
  const char *n;
};

int main(int argc, char **argv) {
  a x("hello");
  b y = x;
  std::string z = (const char*)y;
  std::cout << y;
  std::cout << (b)x;
  std::cout << (const char*)x;// would fail
  return 0;
}
