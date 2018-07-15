// from https://stackoverflow.com/questions/51342721/search-for-insert-position-using-c11-range-based-for-loop
#include <iostream>
#include <list>


template<typename T>
class mylist : public std::list<T> {

    typedef typename std::list<T> _basec;
    typedef typename std::list<T>::iterator _iter;
public:
    mylist(std::initializer_list<T> l) : _basec(l)
    {};

    class proxy_it {
    public:
	proxy_it(_iter _it) : it(_it) {};

	_iter &operator *() {
	    return it;
	}
	proxy_it &operator ++ () {
	    ++it;
	    return *this;
	}
	bool operator != (const proxy_it &a) {
	    return a.it != this->it;
	}
	_iter &it;
    };

    class proxy {
    public:
	proxy(_basec &b) : b(b) {};
	_basec &b;
	proxy_it begin() { proxy_it(b.begin()); };
	proxy_it end() { proxy_it(b.end()); };
    };


};

int main(int argc, char **argv) {

    mylist<int> l = { 1, 2, 3, 4};
    mylist<int>::proxy p{ l };


    for (auto &i: mylist<int>::proxy( l )) {
	if (*i == 3) {
	    l.insert(i, 3);
	    break;
	}
    }

    for (auto i : l) {
	std::cout << " " << i;
    }
};
