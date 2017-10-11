/* https://stackoverflow.com/questions/213761/what-are-some-uses-of-template-template-parameters-in-c/14311714#14311714 */
#include <stdio.h>
#include <iostream>
#include <functional>

#include <iostream>
#include <vector>
#include <deque>
#include <list>
using namespace std;

/*typedef const int & ref0;
typedef int const & ref1;
*/

typedef const int *& ref0;
typedef int *const & ref1;

int *b0;
ref1 b = b0;

const int *a0;
ref0 a = a0;

const int c = 10;

/*
int data(ref0 r0, ref1 r1) {
  std::cout << __PRETTY_FUNCTION__ << "\n";
  }*/

int main()
{
  int *v0, *v1;
  //data(v0,v1);
  return 0;
}
