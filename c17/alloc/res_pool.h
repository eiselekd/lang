#ifndef _RES_POOL_HEADER_H_
#define _RES_POOL_HEADER_H_

#include "res.h"
#include "strprintf.h"

struct pool : resource
{
    pool(pool *p = nullptr, bool isstack=false) : resource(p, isstack) {};
    virtual ~pool() { release(); };
    std::string name;
    union llist<resource, &resource::n> inside;
    void addResource(resource &m) { inside.add_head(m.n); }
    void release(void);

    size_t size() {
	return inside.size();
    }

};

struct rootPool : pool {
    rootPool() : pool(this, true) {};

    std::string str() {
	return vformat ("r:%d", inside.size());
    }

};

extern rootPool grootPool;
#endif
