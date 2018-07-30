#include <iostream>
#include <memory>
using namespace std;

int
main(int argc, char **argv) {

    static void* table[] = {
        &&val1, &&val2 };

    goto *table[0];

val1:
    printf("val1\n");
    goto *table[1];

val2:
    printf("val2\n");

    return 0;
}
