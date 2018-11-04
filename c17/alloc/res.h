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

struct VROOTPOOL;
struct pool;
struct resource
{
    resource(pool *p = nullptr, bool isstack=false);
    virtual ~resource() { n.rem_node(); }

    lnode<resource> n;
    bool isstack() { return isstack_; };

    bool isstack_;

    virtual void dump(const resource &) { };
    virtual resource *lookup(const resource *, unsigned long) { return 0; };
    virtual size_t memsize(const resource &) { return 0; };

    //static void operator delete(void* ptr, std::size_t sz);

};



struct VROOTPOOL {
    union llist<resource, &resource::n> inside;
};




#endif
