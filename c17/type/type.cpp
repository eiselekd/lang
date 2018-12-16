#include <iostream>
#include <map>

struct method {

};

struct classdef {
    struct classdef *super;
};

method *lookup(classdef &c, int id) {
    classdef *_c = &c;
    std::map <int,method *> tbl;

    while (auto i = c.tbl.find(id), i != c.tbl.end()) {

    }

	(i = c.tbl.find(id)) != c.tbl.en()) {


	auto i = c.tbl.find(id);
	if (i == c.tbl.end())
	{

	    continue;
	auto i = c.tbl.find(id)
    }
};
