#ifndef L8_H_DLLIST
#define L8_H_DLLIST

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
        lnode<b> *head;
        lnode<b> *null;
        lnode<b> *tail;
    };

    llist()
    {
	this->head = &this->tail_node;
	this->null = nullptr;
	this->tail = &this->head_node;
    }

    void
    add_head(lnode<b> &n)
    {
	lnode<b> *z = this->head;

	n.next = z;
	n.prev = &(this->head_node);
	z->prev = &n;
	this->head = &n;
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

    lit begin() const { return lit(this->head); }
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

	lit_save begin() const { return lit_save(l->head); }
	lit_save end() const { return lit_save(l->tail->next); }

	llist_type *l;
    };
    llist_save saveit() { return llist_save(this); };
    /* ^^^^^ delete save iterator ^^^^^ */


};

#endif
