#include <stdio.h>
#include <iostream>
#include <functional>
using namespace std;
using namespace std::placeholders;

int add(int a, int b) {
  return a+b;
}

int main(int argc, char **argv) {
  function<int (int) > add4 = bind(&add, 4, _1);
  cout << add4(5) << endl;
  return 0;
}
