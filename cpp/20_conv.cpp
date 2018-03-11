#include <stdio.h>
#include <stdlib.h>

typedef int hash_t;

struct wrap {
    int v;
    operator hash_t () {
	hash_t h = v+1;
	return h;
    }
};

int main(int argc, char **argv) {
    int v = 1;
    hash_t h = (hash_t)wrap{v};
    printf("%d\n", h);
    return 0;
}
