#ifndef _RES_SLAB_HEADER_H_
#define _RES_SLAB_HEADER_H_

#include <string>
#include <unistd.h>
#include "dllist.h"
#include "ulist.h"
#include "alloc.h"
#include "stackplace.h"
#include "res.h"
#include "res_pool.h"
#include <cstddef>
#include <cassert>

struct slbhead;
struct slbobj
{
    slbobj(slbhead *up) : up_(up) {};
    slbhead *up_;
    union
    {
	unode<slbobj> n;
	unsigned char d_[0];
    };
    static slbobj *container_of(void *p)
    {
	return (slbobj *) (((unsigned char*)p) - (long)&(((slbobj*)0)->d_));
    }
};

struct slbhead
{
    slbhead() : numfull_(0) { };
    lnode<slbhead> n;
    ulist<slbobj,&slbobj::n> free;
    size_t numfull_;
};


struct sl_alignment
{
    int8_t data;
    int x[0];
};

template <int SLAB_SIZE, int MAX_EMPTY_HEADS=16>
struct slab : resource {
    slab(resclass *r, pool *p, size_t size) : resource(r,p), objSize_(size) {
	initSizes(size);
    };
    slab(size_t size) : resource(nullptr), objSize_(size) {
    	initSizes(size);
    };
    virtual ~slab() {

    }

    stl_allocator<slbhead> alloc_;

    void initSizes(size_t size)
    {
	uint align = sizeof(struct sl_alignment);
	if (align < sizeof(int))
	    align = sizeof(int);
	dataSize_ = size;
	size += offsetof(slbobj, d_);
	if (size < sizeof(slbobj))
	    size = sizeof(slbobj);
	size = (size + align - 1) / align * align;
	objSize_ = size;
	headSize_ = (sizeof(slbhead) + align - 1) / align * align;
	objsPerSlab_ = (SLAB_SIZE - headSize_) / size;
	assert(objsPerSlab_);
	numEmptyHeads_ = 0;
    }

    slbhead *sl_new_head()
    {
	slbhead *h = alloc_.newObjInter(SLAB_SIZE);
	slbobj *o = reinterpret_cast<slbobj *>((uint8_t*)h+headSize_);
	uint n = objsPerSlab_;
	while (n--) {
	    new(o) slbobj(h);
	    h->free.add_head(o->n);
	    o = reinterpret_cast<slbobj *>((uint8_t*)o+objSize_);
	}
	return h;
    };

    void freeSlab(void *p)
    {
	slbobj *o = slbobj::container_of(p);
	slbhead *h = o->up_;
	h->free.add_head(o->n);
	if (!--h->numfull_) {
	    h->n.rem_node();
	    if (numEmptyHeads_ >= MAX_EMPTY_HEADS)
		alloc_.relObj(h);
	    else
	    {
		emptyHeads_.add_head(h->n);
		numEmptyHeads_++;
	    }
	}
    }

    void *allocSlab()
    {
	slbhead *p = nullptr;
	slbobj *o;
	do
	{
	    /* partial */
	    if (partialHeads_.empty())
	    {
		if (emptyHeads_.empty())
		{
		    /* new head */
		    p = sl_new_head();
		} else
		    p = emptyHeads_.rem_head();
		partialHeads_.add_head(p->n);
	    } else
		p = partialHeads_.head();

	    assert(p);
	    if (p->free.empty())
	    {
		assert(p == partialHeads_.head());
		partialHeads_.rem_head();
		fullHeads_.add_tail(p->n);
		p = nullptr;
	    }
	}
	while( !p);

	o = p->free.rem_head();
	return o->d_;
    }

    uint objSize_, headSize_, objsPerSlab_, numEmptyHeads_, dataSize_;
    llist<slbhead,&slbhead::n> emptyHeads_, partialHeads_, fullHeads_;
};

template <int SLAB_SIZE, int MAX_EMPTY_HEADS=16>
struct slab_resclass : resclass {

    typedef slab<SLAB_SIZE,  MAX_EMPTY_HEADS> slabtyp;
    stl_allocator<slabtyp> alloc_;

    slabtyp *slabAlloc(size_t size, pool *p = nullptr)
    {
	slabtyp *r = alloc_.newObj(size, this, p, size);
	return r;
    }
    void slabFree(slabtyp *r)
    {
	delete(r);
    }

    virtual void dump(const resource &)
    {

    }
    virtual resource *lookup(const resource *, unsigned long)
    {
	return 0;
    }
    virtual size_t memsize(const resource &)
    {
	return 0;
    }

};

#endif
