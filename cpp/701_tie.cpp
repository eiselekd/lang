#include <map>
#include <tuple>
#include <string>
#include <iostream>
using namespace std;

int main(int argc, char **argv) {
    int k0 = -1, k1; string v0, v1;
    map<int , string> m;

    m[1] = "Hello";
    m[2] = "World";

    tie(k0,v0) = *(m.find(1));
    tie(k1,v1) = *(m.find(2));

    if (m.count(3)) {
	tie(k0,v0) = *(m.find(3));
	cout << k0 << ":" << v0 << endl;
    }

    return 0;
}
