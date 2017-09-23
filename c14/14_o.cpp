#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>

class a {
public:
  a(std::initializer_list<int> a) {
  }
};

class c {
public:
  int e0 = 2;
  int e1 = 3;
};

class b {
public:
  b() : b0{0,1} {
  }
  b(std::initializer_list<int> l) : b0{*l.begin(),2}{
  }
  struct c b0;
  int d0 = 1;
  int d1 = 2;
};

class d {
public:
  d(int a, int b) {};
  int e0 = 2;
  int e1 = 3;
};

int main(int argc, char **argv) {
  a x({1,2,3,4});
  b y{1,2};
  d z{1,2};
  return 0;
}
