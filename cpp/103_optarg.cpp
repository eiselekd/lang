#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <iostream>

int main(int argc, char **argv) {

    int c, verbose = 0;
    const char *fn = "test";
    while ((c = getopt (argc, argv, "vf:")) != -1)
	switch (c)
	{
	case 'f':
	    fn = optarg;
	break;
	case 'v':
	    verbose++;
	    break;
	}
    argv += optind;
    argc -= optind;

    std::cout << fn;
    return 0;
}
