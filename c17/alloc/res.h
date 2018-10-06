#include <string>
#include <unistd.h>
#include "dllist.h"
#include "alloc.h"

/*
* resourceclass
** a resource is part of one pool
**

* #
*/

struct resource;
struct resclass
{
    std::string name;
    unsigned size;
    virtual void free(resource &);
    virtual void dump(const resource &);
    virtual resource *lookup(const resource *, unsigned long);
    virtual size_t memsize(const resource &);
};

struct resource
{
    lnode<resource> n;
    struct resclass *rclass;
};

struct pool : resource
{
    std::string name;
    union llist<resource, &resource::n> inside;
    void addResource(resource &m) { inside.add_head(m.n); }
};

struct mblock : resource {
    mblock(size_t size) : size_(size) {};

    static mblock *container_of(void *p) {
	return (mblock *) (((unsigned char*)p) - (long)&(((mblock*)0)->data_));
    }

    size_t size_;
    uintptr_t data_align[0];
    unsigned char data_[0];
};

struct mb_resclass : resclass {
    stl_allocator<mblock> alloc_;
    void *mballoc(pool &p, size_t size) {
	mblock *m;
	m = alloc_.newObj(size, size);
	p.addResource(*m);
	return m->data_;
    }
    void mbfree(void *p) {
	mblock *m;
	m = mblock::container_of(p);
	alloc_.relObj(m);
    }

    virtual void free(resource &) {
    }
    virtual void dump(const resource &) {
    }
    virtual resource *lookup(const resource *, unsigned long) {
    }
    virtual size_t memsize(const resource &) {
    }

};
