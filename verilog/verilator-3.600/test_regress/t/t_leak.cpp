// $Id$
// DESCRIPTION: Verilator: Verilog Test driver/expect definition
//
// Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.

#include <stdlib.h>
#include <stdio.h>
#include <verilated.h>
#include "Vt_leak.h"

unsigned int main_time = false;
double sc_time_stamp () {
    return main_time;
}

long long get_memory_usage() {
    // Return memory usage.  Return 0 if the system doesn't look quite right.

#if 0 // BSD only.
    struct rusage usage;
    getrusage(RUSAGE_SELF, &usage);
    return usage.ru_ixrss + usage.ru_idrss + usage.ru_isrss;
#endif

    FILE* fp = fopen("/proc/self/stat", "r");
    if (!fp) return 0;

    int		ps_ign;
    long long	ps_vsize, ps_rss;
    int items = fscanf(fp, ("%d (%*[^) ]) %*1s %d %*d %*d %*d %*d %u"
			    " %u %u %u %u %d %d %d %d"
			    " %*d %*d %*u %*u %d %llu %llu "),
		       &ps_ign, &ps_ign, &ps_ign,
		       &ps_ign, &ps_ign, &ps_ign, &ps_ign,
		       &ps_ign, &ps_ign, &ps_ign, &ps_ign,
		       &ps_ign, &ps_vsize, &ps_rss);
    fclose(fp);
    if (items >= 14) {
	return ps_vsize;
    } else {
	return 0;
    }
}

void make_and_destroy () {
    Vt_leak* topp = new Vt_leak;

    Verilated::debug(0);
    Verilated::gotFinish(0);
    topp->eval();
    topp->clk = true;
    while (!Verilated::gotFinish()) {
	main_time+=5;
	topp->clk=!topp->clk;
	topp->eval();
    }

    delete topp; topp=NULL;
}

int main (int argc, char *argv[]) {
    long long firstUsage = get_memory_usage();

    // Warmup phase
    for (int i=0; i<1000; i++) {
	make_and_destroy();
    }
    firstUsage = get_memory_usage();
    printf("Memory size %lld bytes\n", firstUsage);

    int loops = 100*1000;
    for (int left=loops; left>0;) {
	for (int j=0; j<1000; j++, left--) {
	    make_and_destroy();
	}
    }

    long long leaked = get_memory_usage() - firstUsage;
    if (leaked > 64*1024) {  // Have to allow some slop for this code.
	printf ("Leaked %lld bytes, or ~ %lld bytes/construt\n", leaked, leaked/loops);
	vl_fatal(__FILE__,__LINE__,"top", "Leaked memory\n");
    }

    printf ("*-* All Finished *-*\n");
}
