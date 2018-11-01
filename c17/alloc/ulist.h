#ifndef L8_H_ULIST
#define L8_H_ULIST

#include <cassert>

template <typename b>
struct unode {

    unode() : next(nullptr) {};
    unode<b> *next;

    void
    rem_node()
    {
	if (next == nullptr)
	    return;

	this->next = nullptr;
    }
};

template <typename b, unode<b> (b::*p)>
union ulist {

    typedef unode<b> unodetyp;
    typedef ulist<b,p> ulisttyp;

    unodetyp *head_;

    ulist()
    {
	this->head_ = nullptr;
    }

    void
    add_head(unodetyp &n)
    {
	n.next = this->head_;
	this->head_ = &n;
    }

    unodetyp *head() {
	return head_;
    }

    bool empty() {
	return head_ == nullptr;
    }

    b *
    rem_head() {
	unodetyp *e = head_;
	assert(e);
	head_ = e->next;
	return container_of(*e);
    }

    static b *container_of(const unode<b> &ptr) {
	return (b*) (((char*)&ptr) - (long)&(((b*)0)->*p));
    }

    struct lit {
	lit(const unodetyp *i) : i(i) {}
	lit & operator++()
	{
	    i = i->next;
	    return *this;
	}
	bool operator!=(const lit &that) const
	{
	    return i != that.i;
	}
	b &operator*()
	{
	    return *container_of(*i);
	}
	const unodetyp *i;
    };

    lit begin() const { return lit(this->head); }
    lit end() const { return lit(nullptr); }
};

#endif
