#include <stdio.h>
#include "alloc.h"
#include "dllist.h"

struct resclass {
    int size;
};

struct res {
    lnode<res> n;
    struct resclass *c;

};

struct respool {
    struct res r;
    llist<res, &res::n> inside;
};


int main(int argc, char **argv) {


    return 0;
}
