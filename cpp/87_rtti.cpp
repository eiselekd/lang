#include <iostream>
#include <typeinfo>
#include <typeindex>
using namespace std;

struct base { virtual void f(void) = 0; };
struct a : public base { int v; void f(void) {}; };
struct b : public base { int v; void f(void) {}; };

void f(base *v) {
    cout << typeid(*v).name() << "\n";
    switch(typeid(*v).hash_code()) {
    case typeid(a).hash_code():
	cout << "Is a\n";
	break;
    case typeid(b).hash_code():
	cout << "Is b\n";
	break;
    }
}

int main(int argc, char **argv) {

    a v0;
    b v1;
    f(&v0);
    f(&v1);

    return 0;
}
