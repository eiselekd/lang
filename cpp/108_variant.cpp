#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>
#include <new>
#include <new>
#include <cstddef>
#include <cassert>
#include <type_traits>
#include <vector>
#include <functional>
#include <variant>

struct success { char *content; };
struct failed  {};

using var1 = std::variant< success, failed >;

/* https://www.youtube.com/watch?v=3KyW5Ve3LtI */

var1 open(const char *f) {
    var1 v{failed{}};
    return v;
}

int main(int argc, char **argv) {

    struct {
	int v;
	auto operator()(const failed &f) const {
	    printf("Failed %d", v);
	}
	auto operator()(const success &f) const {
	    printf("Success");
	}
    } visit {1};
    std::visit(visit, open("/tmp/test.txt"));

    return 0;
}
