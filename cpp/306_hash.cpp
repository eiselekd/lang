#include <iostream>
#include <memory>
#include <map>

struct method {
    virtual ~method() { std::cout << "f\n"; };
};
typedef std::unique_ptr<method> MPTR;

std::map<int, MPTR> tbl;

void insert(int id, method *m) {
    tbl.insert({id,std::unique_ptr<method>(m)});
};

void set(int id, method *m) {
    tbl[id] = std::unique_ptr<method>(m);
};

int main(int argc, char **argv) {

    insert(1,new method());
    set(1,new method());
    return 0;
}
