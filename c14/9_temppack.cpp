#include <stdio.h>
#include <iostream>
#include <stdlib.h>

template<typename TType> void print(const TType& v) {
  std::cout << v << " ";
}

template<typename TType, typename... TTypes> void print(const TType& v, TTypes&&... elements) {
  print(v);
  print(elements...);
}

int main(int argc, char **argv) {
  print("Hello", 1, "world", 2.1);
  return 0;
}
