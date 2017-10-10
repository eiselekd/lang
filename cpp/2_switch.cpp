#include <stdio.h>
#include <iostream>
#include <functional>
using namespace std;
using namespace std::placeholders;

int main(int argc, char **argv) {
  static int state = 0, i, out = 0, inout = 0;
  while (!out) {
  restart:
    switch(state) {
    case 0:
      for (i = 0; i < 16; i++) {
	printf("%i ", i);
	state = 1;
	goto restart;
      case 1: 1;
      }
      state = 2;
      break;
    case 2: 1;
      if (inout) {
	state = 3;
	goto restart;
      case 3: 1;
      } else {
	state = 4;
	goto restart;
      case 4: 1;
	printf("\n");
      }
      out = 1;
    }
  }
  return 0;
}
