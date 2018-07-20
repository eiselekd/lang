#include <stdio.h>
#include <iostream>
#include <cstddef>

template <typename a, typename b>
struct nod {
    nod *next, *prev;
    operator b*() {
	printf("Next %p\n", this);
	return (b*) (this->next);
    }
};


template <typename a, typename b>
struct llist {

    typedef nod<a,b> node;

    struct {
	node head_node;
	void *head_padding;
    };
    struct {
	void *tail_padding;
	node tail_node;
    };
    struct {
	node *head;
	node *null;
	node *tail;
    };

    operator b*() {
	return (b*) (this->head);
    }
};

template <typename a, typename b>
void
add_head(llist<a,b> &l, typename llist<a,b>::node &n)
{
    typename llist<a,b>::node *z = l.head;

    n.next = z;
    n.prev = &l.head_node;
    z->prev = &n;
    l.head = &n;
}

template <typename a, typename b>
void
rem_node(typename llist<a,b>::node &n)
{
    auto *z = n.prev;
    auto *x = n.next;

    z->next = x;
    x->prev = z;
    n.next = NULL;
    n.prev = NULL;
}

template <typename a, typename b>
void
init_list(llist<a,b> &l)
{
    l.head = &l.tail_node;
    l.null = NULL;
    l.tail = &l.head_node;
}

struct c0;
struct c1 {
    llist<c0, c1>::node n;
};

struct c0 {
    llist<c0, c1> l;
};

int main(int argc, char **argv) {

    c0 v0;
    c1 e0, e1, e2;
    c1 *i0;

    printf("%p %p %p\n", &e0, &e1, &e2);

    init_list(v0.l);

#define WALK_LIST(i,list) for(i=list; i->n.next; i=i->n)

    add_head(v0.l, e0.n);
    WALK_LIST(i0,v0.l) {
	std::cout << "0:" << i0 << "\n";
    }
    std::cout << "\n";

    add_head(v0.l, e1.n);
    add_head(v0.l, e2.n);


    WALK_LIST(i0,v0.l) {
	std::cout << i0 << "\n";
    }


    rem_node<c0,c1>(e1.n);
    std::cout << "\n";

    WALK_LIST(i0,v0.l) {
	std::cout << i0 << "\n";
    }

    return 0;
}
