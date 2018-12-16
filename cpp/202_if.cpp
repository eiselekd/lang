#include <map>

struct a {
    a *up;
    std::map<int,int> tbl;
};

void p(a *_a, int id) {
    decltype(_a->tbl)::iterator i;
    while (_a && ((i = _a->tbl.find(id)) != _a->tbl.end())) {
	i->second += 1;
	_a = _a->up;
    }
}

int main(int arc, char **argv) {
    a _a1{0,{{0,10},{1,10}}};
    a _a0{&_a1,{{2,11},{3,11}}};
    p(&_a0, 0);
    return 0;
}
