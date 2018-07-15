#include <iostream>
#include <list>

class b {
public:
    int v;
};

class a {
public:
    operator b() {
	printf("Convert to b\n");
	return base; };
    b base;
};


int main(int argc, char **argv) {
    a v0;
    b v1;
    b v2 = v0;
};
