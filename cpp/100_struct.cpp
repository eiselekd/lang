#include <string>

struct a {
    std::string w;
    int v;
};

struct a f() {
    return (struct a) { std::string("v"), 1 };
}

int main(int argc, char **argv) {
    return 0;
}
