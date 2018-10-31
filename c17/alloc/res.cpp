#include <stdio.h>
#include "dllist.h"
#include "res_pool.h"

resource::resource(pool *p) {
    if (p)
	p->addResource(*this);
}

void pool::release() {
    for (auto &v: inside.saveit()) {
	delete(&v);
    }
}
