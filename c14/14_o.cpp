#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <functional>
#include <stdio.h>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <functional>

class a {
public:
  a(std::initializer_list<int> a) {
  }
};

class c {
public:
  int e0 = 2;
  int e1 = 3;
};

class b {
public:
  b() : b0{0,1} {
  }
  b(std::initializer_list<int> l) : b0{*l.begin(),2}{
  }
  struct c b0;
  int d0 = 1;
  int d1 = 2;
};

class d {
public:
  d(int a, int b) {};
  int e0 = 2;
  int e1 = 3;
};

int
func(int a = 1, int b = 2) {
  return 0;
}

class c0 {
public:

};

class c1 : c0 {
public:
  void operator=(const c0 &a) {}
};

class mypair {
public:
  mypair(int i, std::string a) {}
};

class mymap {
public:
  mymap(std::initializer_list<mypair> a) {
    for (auto i: a) {
      insert(i);
    }
  }
  void insert(mypair &p) {
  }
  void insert(mypair &&p) {
  }
};

int main(int argc, char **argv) {
  a x({1,2,3,4});


  b y{1,2};
  d z{1,2};
  func(2);
  c1 x0;
  x0=c1{};
  std::map<int,std::string> a({{1,"test0"}, {2, "test1"}});

  mymap m0({{1,"test0"},{2,"test1"}});

  m0.insert(mypair(1,"test3"));


  return 0;
}
