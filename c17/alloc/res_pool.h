#ifndef _RES_POOL_HEADER_H_
#define _RES_POOL_HEADER_H_

#include "res.h"

struct pool_resclass : resclass {
    int dummy;
};

struct pool : resource
{
    pool(resclass *r) : resource(r) {};
    std::string name;
    union llist<resource, &resource::n> inside;
    void addResource(resource &m) { inside.add_head(m.n); }
    void release(void);
};


#endif
