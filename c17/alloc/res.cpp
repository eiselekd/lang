#include <new>
#include <cstdio>
#include <cstdlib>
#include "dllist.h"
#include "res_pool.h"

rootPool grootPool;

resource::resource(pool *p, bool isstack) : isstack_(isstack) {
    if (!p)
	p = &grootPool;
    if (p && p != this) /* skip rootpool */
	p->addResource(*this);
}

void pool::release() {

    for (auto &v: inside.saveit()) {
	if (!v.isstack()) {// if stack call destructor on exit
	    /* assume that constructor called with placement new or class operator new */
	    (&v)->~resource();
	    free(&v);
	}
    }
}
