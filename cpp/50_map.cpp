#include <map>
#include <iostream>

int main(int argc, char**argv) {

  std::map< int, std::string > testing  { {1,"2"},{3,"4"}  };
  for ( const auto& [ k, v ] : testing ) {
    std::cout << k << "=" << v << "\n";
  }
  return 0;
}
