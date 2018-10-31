#ifndef _RES_HEADER_H_
#define _RES_HEADER_H_

#include <string>
#include <unistd.h>
#include "dllist.h"
#include "alloc.h"

/*
* resourceclass
** a resource is part of one pool
** free is called via virtual destructor

* #
*/

struct resource;
struct resclass
{
    std::string name;
    unsigned size;
    virtual void dump(const resource &) = 0;
    virtual resource *lookup(const resource *, unsigned long) = 0;
    virtual size_t memsize(const resource &) = 0;
};

struct pool;
struct resource
{
    resource(pool *p = nullptr);
    virtual ~resource() { n.rem_node(); }

    lnode<resource> n;
    struct resclass *rclass;
};



#endif
