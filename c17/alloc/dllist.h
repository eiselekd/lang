#ifndef L8_H_DLLIST
#define L8_H_DLLIST

#include <stddef.h>

template <typename b>
struct lnode {

    lnode() : next(nullptr), prev(nullptr) {};

    lnode<b> *next, *prev;

    void
    rem_node()
    {
	if (next == nullptr || prev == nullptr)
	    return;

	lnode<b> *z = this->prev;
	lnode<b> *x = this->next;

	z->next = x;
	x->prev = z;
	this->next = nullptr;
	this->prev = nullptr;
    }
};

template <typename b, lnode<b> (b::* p)>
union llist {

    typedef llist<b,p> llist_type;

    struct {
        lnode<b> head_node;
        void *head_padding;
    };
    struct {
        void *tail_padding;
        lnode<b> tail_node;
    };
    struct {
        lnode<b> *head_;
        lnode<b> *null;
        lnode<b> *tail;
    };

    llist()
    {
	this->head_ = &this->tail_node;
	this->null = nullptr;
	this->tail = &this->head_node;
    }

    bool empty() {
	return head_ == tail->next;
    }

    b *
    head()
    {
	return container_of(*head_);
    }

    b *
    rem_head(void) {
	lnode<b> *e = this->head_;
	e->rem_node();
	return container_of(*e);
    }


    void
    add_head(lnode<b> &n)
    {
	lnode<b> *z = this->head_;

	n.next = z;
	n.prev = &(this->head_node);
	z->prev = &n;
	this->head_ = &n;
    }
    void
    add_tail(lnode<b> &n)
    {
	lnode<b> *z = this->tail;

	n.next = &(this->tail_node);
	n.prev = z;
	z->next = &n;
	this->tail = &n;
    }

    size_t
    size() {
	size_t j = 0;
	for (auto &i:*this) j++;
	return j;
    }

    static b *container_of(const lnode<b> &ptr) {
	return (b*) (((char*)&ptr) - (long)&(((b*)0)->*p));
    }

    struct lit {
	lit(const lnode<b> *i) : i(i) {}

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
	const lnode<b> *i;
    };

    lit begin() const { return lit(this->head_); }
    lit end() const { return lit(this->tail->next); }

    /* vvvvv delete save iterator vvvvv */
    struct llist_save {
        llist_save(llist_type *l) : l(l) {};
	struct lit_save {
            lit_save(const lnode<b> *i) : i(i), n(i->next) {}
	    lit_save & operator++()
	    {
		i = n;
		n = n->next;
		return *this;
	    }
	    bool operator!=(const lit_save &that) const
	    {
		return i != that.i;
	    }

	    b &operator*()
	    {
		return *container_of(*i);
	    }
	    const lnode<b> *i;
	    const lnode<b> *n;
	};

	lit_save begin() const { return lit_save(l->head_); }
	lit_save end() const { return lit_save(&l->tail_node); }

	llist_type *l;
    };
    llist_save saveit() { return llist_save(this); };
    /* ^^^^^ delete save iterator ^^^^^ */
};

#ifdef MAIN_TEST_DLLIST
#include <cassert>

struct mobj
{
    int id;
    lnode<mobj> n;
};

struct mhead
{
    int dummy2;
    int dummy3;
    llist<mobj,&mobj::n> h;
};


int main(int argc, char **argv)
{
    {
	mobj o0{0}, o1{1}, o2{2};
	{
	    mhead h1;

	    h1.h.add_tail(o0.n);
	    h1.h.add_tail(o1.n);
	    h1.h.add_tail(o2.n);
	    int id = 0;
	    for (auto &i: h1.h) {
		assert(i.id == id);
		id++;
	    }
	    assert(3 == id);
	}
	{
	    mhead h0;
	    h0.h.add_head(o2.n);
	    h0.h.add_head(o1.n);
	    h0.h.add_head(o0.n);
	    int id = 0;
	    for (auto &i: h0.h) {
		assert(i.id == id);
		id++;
	    }
	    assert(3 == id);
	}

    }
    return 0;
}
#endif

#endif
