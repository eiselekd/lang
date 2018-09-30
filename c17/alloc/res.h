#include <string>
#include "dllist.h"

struct resource;
struct resclass
{
    std::string name;
    unsigned size;
    virtual void free(resource &);
    virtual void dump(const resource &);
    resource *lookup(const resource *, unsigned long);
    size_t memsize(const resource &);
};

struct resource
{
    struct resclass *rclass;
    lnode<resource> n;

};

struct pool : resource
{
    std::string name;
    union llist<resource, &resource::n> inside;
};
