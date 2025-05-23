// $Id$
// DESCRIPTION: Verilator Test: Top level main for invoking model
//
// Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.

#include <verilated.h>

#include "Vvgen.h"
#include "Vvgen_vgen.h"        // For v

Vvgen *top;

unsigned int main_time = 0;

double sc_time_stamp () {
    return main_time;
}

int main(int argc, char **argv, char **env) {
    Verilated::debug(0);	// We compiled with it on for testing, turn it back off

    top = new Vvgen;
    top->check = 0;
    top->clk = 0;

#define CYCTIME 10

    // Cycle the interpreter
    while (main_time < CYCTIME*top->v->CYCLES) {
	top->eval();
	main_time += CYCTIME/2;
	top->clk = !top->clk;
	top->eval();
	main_time += CYCTIME/2;
	top->clk = !top->clk;
    }

    // Do a checking run
    top->check = 1;
    for (int i=0; i<10; i++) {
	top->eval();
	main_time += CYCTIME/2;
	top->clk = !top->clk;
	top->eval();
	main_time += CYCTIME/2;
	top->clk = !top->clk;
    }

    top->final();

    exit(0L);
}
