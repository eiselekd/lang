#include <stdio.h>
#include "alloc.h"
#include "dllist.h"
#include "res.h"
#include "res_mblock.h"

int main(int argc, char **argv)
{
    mb_resclass m0;
    pool pool0;
    auto p0 = m0.mballoc(pool0, 100);
    auto p1 = m0.mballoc(pool0, 1000);
    auto p2 = m0.mballoc(pool0, 10000);
    m0.mbfree(p0);
    m0.mbfree(p1);
    m0.mbfree(p2);
    return 0;
}
