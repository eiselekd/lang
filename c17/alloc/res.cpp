#include <stdio.h>
#include "dllist.h"
#include "res_pool.h"

resource::resource(resclass *r, pool *p, bool isstack) : rclass(r), isstack_(isstack) {
    if (!p)
	p = globalPool();
    if (p)
	p->addResource(*this);
}

void pool::release() {
    for (auto &v: inside.saveit()) {
	if (!v.isstack()) // if stack call destructor on exit
	    delete(&v);
    }
}

pool *
globalPool()
{
    return 0;
}
