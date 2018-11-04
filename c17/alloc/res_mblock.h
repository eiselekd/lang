#ifndef _RES_MBOCK_HEADER_H_
#define _RES_MBOCK_HEADER_H_

#include <string>
#include <unistd.h>
#include "dllist.h"
#include "alloc.h"
#include "stackplace.h"
#include "res.h"
#include "res_pool.h"

struct mblock : resource {
    mblock(pool *p, size_t size) : resource(p), size_(size) {};
    mblock(size_t size) : resource(nullptr), size_(size) {};
    virtual ~mblock() { }
    static mblock *container_of(void *p) {
	return (mblock *) (((unsigned char*)p) - (long)&(((mblock*)0)->data_));
    }
    size_t size_;
    uintptr_t data_align[0];
    unsigned char data_[0];
};

struct mb_resclass  {
    stl_allocator<mblock> alloc_;
    void *mballoc(pool &p, size_t size) {
	mblock *m;
	m = alloc_.newObj(size, &p, size);
	return m->data_;
    }
    void mbfree(void *p) {
	mblock *m;
	m = mblock::container_of(p);
	alloc_.relObj(m);
    }

};

#endif
