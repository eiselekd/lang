#include <vector>
#include <iostream>

class a {
    typedef std::vector<int> vec;
public:
    vec a;
    void resize(void) {
	vec b(std::move(a));
	vec c(10);
	c.push_back(1);
	c.push_back(2);
	c.push_back(3);
	std::cout << c.size() << std::endl;
	a = std::move(c);
	c.push_back(0);
	std::cout << c.size() << std::endl;
    }
};

int main() {
    a v;
    v.resize();
    return 0;
}
