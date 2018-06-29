#include <stdio.h>
#include <stdlib.h>
#include "hash1.h"
#include <sstream>

int
main(int argc, char **argv)
{
    CHash h;

    for (int i = 0; i < 16; i++) {
	stringstream s; s << i;
	h.push(s.str(),i);
    }

    return 0;
}
