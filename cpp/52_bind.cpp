#include <map>
#include <iostream>


int main(int argc, char **argv) {
  std::map<int,int> v0{{1,2},{3,4},{5,6}};
  for (auto & [i,j] : v0) {
    std::cout << i << "," << j << std::endl;
  }
  return 0;
}
