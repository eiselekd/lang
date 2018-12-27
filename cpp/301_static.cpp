#include <map>
#include <functional>

struct b_c {
    b_c() {
	printf("Init b_c\n");
    }
};

struct a0 {
    a0() : f0_f(f0) {
    };
    std::function<void()> f0_f;
    static void f0(void) { printf("f0 %d\n", m); };
    static int m;

    static b_c *test() {
	static b_c s;
	return &s;
    }



};
int a0::m = 1;

int main(int arc, char **argv) {
    a0 v0;
    v0.f0();
    v0.f0_f();
    a0::f0();
    v0.test();
    a0::test();


    return 0;
}
