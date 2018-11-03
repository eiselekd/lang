#include <stdio.h>
#include "alloc.h"
#include "dllist.h"
#include "res.h"
#include "res_mblock.h"
#include "res_slab.h"
#include "range.h"
#include <array>
#include <vector>
#include <tuple>
#include <iostream>

int main(int argc, char **argv)
{
    (void) argc; (void) argv;
    mb_resclass m0;
    pool pool0(&m0);
    {
	/*
	mb_resclass m0;
	auto p0 = m0.mballoc(pool0, 100);
	auto p1 = m0.mballoc(pool0, 1000);
	auto p2 = m0.mballoc(pool0, 10000);
	m0.mbfree(p0);
	m0.mbfree(p1);
	m0.mbfree(p2);
	*/
    }
    {
	using slabres4086 = slab_resclass<4096>;
	using slab4096 = slabres4086::slabtyp;
	slabres4086 o0; slab4096 *e0;
	std::array<std::tuple<slab4096 *,std::array<void*,1024>>,2> pa;
	for (auto i: range(pa.size())) {
	    e0 = o0.slabAlloc(24+i);
	    std::get<0>(pa[i]) = e0;
	    int k = 0;
	    for (auto j: range(std::get<1>(pa[i]).size())) {
		std::get<1>(pa[i])[j] = e0->allocSlab();
		k++;
		std::cout << k << ":" << e0->str() << "\n";
	    }
	}
	for (auto i: range(pa.size())) {
	    e0 = std::get<0>(pa[i]);
	    for (auto j: range(std::get<1>(pa[i]).size())) {
		e0->freeSlab(std::get<1>(pa[i])[j]);
	    }
	    o0.slabFree(e0);
	}
    }
    return 0;
}
