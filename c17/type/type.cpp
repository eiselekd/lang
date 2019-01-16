#include <iostream>
#include <string>
#include <array>
#include <map>
#include <functional>
#include "method_c.h"

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

    template<class Ld, typename = void>
    void add_c_method(int id, Ld &&f)
    {
	tbl[id] = new method_c(std::forward<Ld>(f));
    };

};

void f1(int a, int b) {
};

struct classdef_object : classdef {
    classdef_object() {
	add_c_method<decltype(f1)>(1, f1);
	add_c_method(2, []() {} );
    }
};

#ifdef _GEN_TYPE_MAIN_

int
main(int argc, char **argv) {
    classdef_object c;
    return 0;
}

#endif

