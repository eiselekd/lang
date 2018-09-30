// https://stackoverflow.com/questions/51428593/howto-write-a-c-style-double-linked-list-implementation-similar-to-kernels-lis

// g++ -g -std=c++11 81_list.cpp -o 81_list.exe
// 8< ---------------- 81_list.cpp ------------------
#include <stdio.h>
#include <iostream>
#include <stdio.h>
#include <iostream>
#include <cstddef>

using namespace std;



/***************************************/
/* definition of double linked list
 * https://github.com/BIRD/bird/blob/470efcb98cb33de2d5636679eb0f72c88280d6b8/lib/lists.h#L12
 */

template <typename b>
struct node {
    node<b> *next, *prev;

    void
    rem_node()
    {
	node<b> *z = this->prev;
	node<b> *x = this->next;

	z->next = x;
	x->prev = z;
	this->next = NULL;
	this->prev = NULL;
    }

};

template <typename b, node<b> (b::* p)>
union llist {
    struct {
        node<b> head_node;
        void *head_padding;
    };
    struct {
        void *tail_padding;
        node<b> tail_node;
    };
    struct {
        node<b> *head;
        node<b> *null;
        node<b> *tail;
    };

    llist()
    {
	this->head = &this->tail_node;
	this->null = NULL;
	this->tail = &this->head_node;
    }

    void
    add_head(node<b> &n)
    {
	node<b> *z = this->head;

	n.next = z;
	n.prev = &(this->head_node);
	z->prev = &n;
	this->head = &n;
    }

    static b *container_of(node<b> &ptr) {
	return (b*) (((char*)&ptr) - (long)&(((b*)0)->*p));
    }

    struct lit {
	lit(node<b> *i) : i(i) {}

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
	node<b> *i;
    };

    lit begin() const { return lit(this->head); }
    lit end() const { return lit(this->tail->next); }
};

/*********************************************/
/* example of usage: */

struct containnode {
    int pad; /* padding allowed */
    node<containnode> n;

    node<containnode> m;
};

struct containlist0 {
    int pad; /* padding allowed */
    llist<containnode, &containnode::n> l;
};

struct containlist1 {
    int pad; /* padding allowed */
    llist<containnode, &containnode::m> l;
};

int main(int argc, char **argv) {

    containlist0 list0;
    containlist1 list1;
    containnode e0, e1, e2;
    containnode *v[3] = { &e0, &e1, &e2 };

    /* add to list */
    for (auto *e : v) {
	list0.l.add_head(e->n);
	list1.l.add_head(e->m);
    }

    /* remove from list0 and print list0 and list1 */
    for (auto *e : v) {
	for (auto &i: list0.l) {
	    cout << &i << "\n";
	}
	cout << "\n";
	e->n.rem_node();

	for (auto &i: list1.l) {
	    cout << &i << "\n";
	}

    }

    return 0;
}
