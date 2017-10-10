#include <stdio.h>
#include <iostream>
#include <functional>
#include <thread>
#include <vector>
#include <future>

int main(int argc, char **argv) {

  auto gen = ([](int a, int b) -> decltype(0) {
      std::cout << "HW:" << a << " " << b <<"\n";
      return 0;
    });
  auto f0 = std::async(std::launch::async,gen,1,2);
  f0.get();
  auto f1 = std::async(std::launch::async,gen,2,3);
  f1.get();
  auto f2 = std::async(std::launch::async,gen,3,4);
  f2.get();
  auto f3 = std::async(std::launch::async,gen,5,6);
  f3.get();
  return 0;
}
