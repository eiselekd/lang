
struct resource;
struct resclass {
    std::string name;
    unsigned size;
    virtual void free(resource &);
    virtual dump(const resource &);
    resource *lookup(const resource *, unsigned long);
    size_t memsize(const resource &);
};

struct resource {
    struct resclass *rclass;
    node<resource> n;

};

struct pool : struct resource {
    std::stirng name;
    union llist<resource, &resource::n> inside;
};
