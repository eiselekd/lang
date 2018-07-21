#include <stdio.h>
#include <iostream>

// http://aszt.inf.elte.hu/~gsd/halado_cpp/ch06s09.html


template <int v>
struct Int2Type
{
    enum { value = v };
};

template <int v, typename a, typename b>
struct SelectType
{
    typedef a result;
};

template <typename a, typename b>
struct SelectType<0,a,b>
{
    typedef a result;
};
template <typename a, typename b>
struct SelectType<1,a,b>
{
    typedef b result;
};

template <typename b>
struct node {
    node<b> *n,*p;
};

class c0 {
public:
    int a;
    int m0;
    node<c0> e;
};


template <typename b, node<b> (b::* p)>
b *container_of(node<b> &ptr) {
    return (b*) ((&ptr) - (long)&(((b*)0)->*p));
}

template <typename b, int b::* p, int idx>
class c1 {
    void p0(Int2Type<0>) { std::cout << "p0"; };
    void p0(Int2Type<1>) { std::cout << "p1"; };
public:
    void print(b &v) {
	std::cout << v.*p;
	std::cout << &(v.*p);
	std::cout << "\noff:";
	std::cout << &(((b*)0)->*p);
	std::cout << "\n";

	p0(Int2Type<idx>());
    }
};

void f0 (int v, c0) { /* unnamed arg */
}

void f0 (int v, int) { /* unnamed arg */
}



int
main(int argc, char **argv)
{
    c0 v0;
    // template with member pointer
    c1<c0, &c0::m0,1> v1;


    c0 *pc0 = container_of<c0,&c0::e>(v0.e);


    SelectType<0,int,double> d0;
    SelectType<1,int,double> d1;

    c0();

    f0(1,c0());
    f0(1,1);

    int c0::* p0 = &c0::m0;

    std::cout << v0.*p0 ;

    v1.print(v0);

    std::cout << "\n";
    return 0;

}
