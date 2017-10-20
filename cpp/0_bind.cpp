#include <stdio.h>
#include <iostream>
#include <vector>
#include <functional>
using namespace std;
using namespace std::placeholders;

/*

* test
** NEXT [#B] test

* dada
  |------+------+----|
  | re   | sdsd | sd |
  | sdsd | sd   |    |


#+BEGIN_SRC C++

int func1() {
  int c = 0;
  return 10;
}

#+END_SRC



#+RESULTS:
[[file:test3.png]]


* #

*/

int add(int a, int b) {

  return a+b;
}

/*
next

* Next chapter
** new functions
* #
*/
int main(int argc, char **argv) {
  function<int (int) > add4 = bind(&add, 4, _1);
  cout << add4(5) << endl;
  return 0;
}
