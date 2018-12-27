#include <iostream>
#include <map>

struct method {

};

struct classdef {
    std::map<int, method*> tbl;
    struct classdef *super;
};

method *
lookup(classdef *c, int id)
{
    decltype(c->tbl)::iterator i;
    while ((i = c->tbl.find(id)) == c->tbl.end()) {
	c = c->super;
	if (!c)
	    return 0;
    }
    return i->second;
};

struct classdef_method : classdef {


};
