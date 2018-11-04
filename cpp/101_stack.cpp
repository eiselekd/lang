#include <stdio.h>
#include <stdlib.h>
#include <string>

class aclass {
public:
    aclass(int size) : size_(size) {};
    int size_;
    char data[0];
};

void f(int size)
{
    std::aligned_storage<sizeof(aclass), alignof(aclass)>::type v;
    new(static_cast<void*>(&v)) aclass(size);
}

int main(int argc, char **argv)
{
    /*
      placement-new:
       http://www.scs.stanford.edu/~dm/home/papers/c++-new.html
       https://stackoverflow.com/questions/222557/what-uses-are-there-for-placement-new
    */

    char *buf  = new char[sizeof(std::string)]; // pre-allocated buffer
    std::string *p = new (buf) std::string("hi");    // placement new
    std::string *q = new std::string("hi");

    f(10);
    f(100);
    f(1000);
    return 0;
}
