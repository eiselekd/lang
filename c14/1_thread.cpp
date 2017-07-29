#include <stdio.h>
#include <iostream>
#include <functional>
#include <thread>
#include <vector>
using namespace std;
using namespace std::placeholders;

namespace bulk {
  class pd {
  public:
    pd & operator << (const char *a) { cout << a; return *this;};
  };
}
bulk::pd g_pd;

std::vector<std::thread> threads;


int gen(std::function<void(bulk::pd&)> pd) {
  threads.push_back(std::thread(pd, std::ref(g_pd)));
}

int main(int argc, char **argv) {

  gen([](auto& g_pd) {
      g_pd << "HW" << "\n";
    });

  for (auto& t : threads)
    t.join();

  return 0;
}
