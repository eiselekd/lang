#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <functional>
#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <functional>

#if defined(DEFINE_A)
class elem_a {
public:
  void func1() {};
  void func2() {};
};

#else

#endif

#if defined(DEFINE_B)
class elem_b {
public:
  void func1() {};
  void func2() {};
};

#else

#endif

class a {
public:
  elem_a a0;
  elem_b a1;
};


int main(int argc, char **argv) {
  a x;
  x.a1.func1();
  x.a0.
  return 0;
}
