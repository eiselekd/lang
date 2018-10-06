#include <stdio.h>
#include <stdlib.h>

class a {
public:
    virtual ~a() {
	printf("a %p\n", this);
    };
};

class b : a {
public:
    virtual ~b() {
	printf("destruct b %p\n", this);
    };
};

class a_nv {
public:
    ~a_nv() {
	printf("a %p\n", this);
    };
};

class b_nv : a_nv {
public:
    ~b_nv() {
	printf("destruct b %p\n", this);
    };
};

int main(int argc, char **argv) {
    {
	a *o0 = new a();
	a *o1 = (a*)new b();
	delete(o0);
	delete(o1);
	a_nv *o0_nv = new a_nv();
	a_nv *o1_nv = (a_nv*)new b_nv();
	delete(o0_nv);
	delete(o1_nv);
    }
    return 0;
}
