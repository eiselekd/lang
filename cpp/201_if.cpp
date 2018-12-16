#include <map>

struct a {
    a *up;
    std::map<int,int> tbl;
};

int lookup(a *_a, int id) {
    decltype(_a->tbl)::iterator i;
    while ((i = _a->tbl.find(id)) == _a->tbl.end()) {
	if (!_a)
	    return -1;
	_a = _a->up;
    }
    return i->second;
}

int main(int arc, char **argv) {
    a _a1{0,{{0,10},{1,10}}};
    a _a0{&_a1,{{2,11},{3,11}}};
    int r = lookup(&_a0, 0);
    return 0;
}
