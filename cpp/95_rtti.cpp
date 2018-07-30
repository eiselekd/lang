#include <iostream>
#include <memory>
using namespace std;

struct base { virtual void f(void) = 0; };
struct a : public base { int v; void f(void) {}; };
struct b : public base { int v; void f(void) {}; };

extern "C" {
//    extern const char _ZTS1a2[];
//    extern const char _ZTS1b2[];
};

void func10(base *v)
{
    int i = typeid(*v).hash_code();
    printf("%x\n", i);
    #  if !__GXX_MERGED_TYPEINFO_NAME
    #error test
    #endif

    /*
    switch((long)p) {
    case (long)atyp:
	break;
	}*/


    /*
    printf("Start %p\n", p);

    switch((long)p) {
    case a:
	break;
    }
    */
}


int
main(int argc, char **argv) {

    a v0;
    b v1;
    func10(&v0);
    func10(&v1);

    return 0;
}
