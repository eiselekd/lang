#include <stdio.h>
#include <iostream>

class c0 {
public:
    int m0;
};

template <typename b, int b::* p>
class c1 {
public:
    void print(b &v) {
	std::cout << v.*p;
    }
};




int
main(int argc, char **argv)
{
    c0 v0{1};
    // template with member pointer
    c1<c0, &c0::m0> v1;

    int c0::* p0 = &c0::m0;

    std::cout << v0.*p0 ;

    v1.print(v0);

    std::cout << "\n";
    return 0;

}
