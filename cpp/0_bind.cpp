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

major-mode minor-mode-alist (string= major-mode 'c++-mode) (get-buffer "*gud*")
(mapcar #'car minor-mode-list)

* [#A] Next chapter
** new functions
* [#B] #
*/
int main(int argc, char **argv) {
  function<int (int) > add4 = bind(&add, 4, _1);
  cout << add4(5) << endl;
  return 0;
}
