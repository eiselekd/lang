#include <map>
#include <iostream>

struct a {
  a(int v) : v(v) { };
  a(const char *v) : v(0) { };
  int v;
};

void f0(std::initializer_list<a> l) {
  for (auto i: l) {
    std::cout << i.v << ",";
  }
  std::cout << std::endl;
}

int main(int argc, char **argv) {
  f0({1,2,3,4,"a"});
  return 0;
}
