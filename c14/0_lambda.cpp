
#include <stdio.h>
#include <iostream>
#include <functional>
using namespace std;

int a(std::function<int (int)> c) {
  c(10);
}

int main() {
  int x = 100;
  auto c1 = [&](int y) {
    cout << x << endl << ":" << y << ":" << endl;
    return x*y>55;
  };
  a(c1) ;
}
