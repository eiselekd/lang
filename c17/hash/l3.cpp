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

};

template <typename iter>
class proxy_it {
public:
    proxy_it(iter it) : it(it) {
    };

    iter &operator *() {
	return it;
    }
    proxy_it &operator ++ () {
	++it;
	return *this;
    }
    bool operator != (const proxy_it &a) {
	return a.it != this->it;
    }
    int c;
    iter &it;
};


template <typename c, typename it>
class proxy_c {
public:
    c &_c;
    it begin() { return {_c.begin()}; };
    it end() { return {_c.end()}; };
};

template <typename c, typename it = typename c::iterator>
proxy_c<c,proxy_it<it>> proxy(c &_c) {
    return {_c};
}

int main(int argc, char **argv) {

    mylist<int> l = { 1, 2, 3, 4};

    for (auto &i: proxy( l )) {
	if (*i == 3) {
	    l.insert(i, 3);
	    break;
	}
    }

    for (auto i : l) {
	std::cout << " " << i;
    }
};
