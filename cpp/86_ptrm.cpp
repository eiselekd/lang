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
 *
 */

template <typename b>
struct node {
    node<b> *next, *prev;

    void
    rem_node()
    {
	next->prev = prev;
	prev->next = next;
    }
};

template <typename b, node<b> (b::* p)>
union llist {
    node<b> head;

    llist()
    {
	head.prev = &head;
	head.next = &head;
    }

    void
    add_head(node<b> &n)
    {
	node<b> *prev, *next;
 	prev = &head;
	next = head.next;

	next->prev = &n;
	n.next = next;
	n.prev = prev;
	prev->next = &n;
    }

    static b *container_of(const node<b> &ptr) {
	return (b*) (((char*)&ptr) - (long)&(((b*)0)->*p));
    }

    struct lit {
	lit(const node<b> *i) : i(i) {}

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
	const node<b> *i;
    };

    lit begin() const { return lit(this->head.next); }
    lit end() const { return lit(&this->head); }
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

	e->n.rem_node();

	cout << "\nlist0:\n";
	for (auto &i: list0.l) {
	    cout << &i << "\n";
	}

	cout << "\nlist1:\n";
	for (auto &i: list1.l) {
	    cout << &i << "\n";
	}

    }

    return 0;
}
