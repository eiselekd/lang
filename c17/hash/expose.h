// from https://stackoverflow.com/questions/51342721/search-for-insert-position-using-c11-range-based-for-loop
#include <iostream>
#include <list>

template <typename iter>
class proxy_it {
public:
    proxy_it(iter it) : it(std::move(it)) {
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
    iter it;
};


template <typename c, typename it>
class proxy_c {
public:
    c &_c;
    it begin() const { return {_c.begin()}; };
    it end() const { return {_c.end()}; };
};

template <typename c, typename it = typename c::iterator>
proxy_c<c,proxy_it<it>> proxy(c &_c) {
    return {_c};
}
