#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>

class b {
public:
  b(std::initializer_list<int> a) {
  }
};

int main(int argc, char **argv) {
  b x({1,2,3,4});

  return 0;
}
