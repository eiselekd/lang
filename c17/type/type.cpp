#include <iostream>
#include <map>
#include <functional>

struct method {
    std::function<void ()> m;
};

struct classdef {
    std::map<int, method*> tbl;
    struct classdef *super;

    void add_method(int id, method *m) {
	decltype(tbl)::iterator i;
	if ((i = tbl.find(id)) != tbl.end()) {

	}
	tbl.insert({id,m});
    }

    method *
    lookup(int id)
    {
	decltype(this) c = this;
	decltype(tbl)::iterator i;
	while ((i = tbl.find(id)) == tbl.end()) {
	    c = c->super;
	    if (!c)
		return 0;
	}
	return i->second;
    };

};

struct classdef_object : classdef {
    classdef_object() {
	add_method([]() { });
    }
};
