#include <stdio.h>
#include <iostream>
#include <stdio.h>
#include <iostream>
#include <cstddef>

template <typename b>
struct node {
    node<b> *next, *prev;
    operator b*() {
        return (b*) (this->next);
    }
};

template <typename a, typename b, int off>
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
    operator b*() {
        return (b*) (this->head);
    }
};

template <typename a, typename b, int off>
void
add_head(llist<a,b, off> &l, node<b> &n)
{
    node<b> *z = l.head;

    n.next = z;
    n.prev = &l.head_node;
    z->prev = &n;
    l.head = &n;
}

template <typename b>
void
rem_node(node<b> &n)
{
    node<b> *z = n.prev;
    node<b> *x = n.next;

    z->next = x;
    x->prev = z;
    n.next = NULL;
    n.prev = NULL;
}

template <typename a, typename b, int off>
void
init_list(llist<a,b, off> &l)
{
    l.head = &l.tail_node;
    l.null = NULL;
    l.tail = &l.head_node;
}


struct c1 {
    node<c1> n;
};

struct c0 {
    llist<c0, c1, offsetof(c1,n)> l;
};

int main(int argc, char **argv) {

    c0 v0;
    c1 e0, e1, e2;
    c1 *i0;

    printf("%p %p %p\n", &e0, &e1, &e2);

    init_list(v0.l);
    add_head(v0.l, e0.n);
    add_head(v0.l, e1.n);
    add_head(v0.l, e2.n);

#define WALK_LIST(i,list) for(i=list; i->n.next; i=i->n)
    WALK_LIST(i0,v0.l) {
        std::cout << i0 << "\n";
    }

    rem_node(e1.n);

    std::cout << "\n";
    WALK_LIST(i0,v0.l) {
        std::cout << i0 << "\n";
    }

    return 0;
}
