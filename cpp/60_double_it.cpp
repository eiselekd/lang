#include <stdio.h>
#include <iostream>
#include <vector>

struct myvec {
  myvec(const std::initializer_list<int> &v) : v(v) { };

  typedef std::pair<int, int> itpair;

  struct range {
    range(const struct myvec *_a, int idx, int len) : a(_a), idx(idx), len(len) {};

    range(const range &r) : a(r.a), idx(r.idx), len(r.len) {};
    // range(const range &&r) : a(r.a), idx(r.idx), len(r.len) {};
    // range& operator =(range&& o) {
    //   a = o.a; idx = o.idx; len = o.len;
    //   return *this;
    // }

    const struct myvec *a;
    int idx, len;

    struct iter1 {

      iter1(const myvec *_a, int idx, int len) : a(_a), idx(idx), len(len) {};
      itpair && operator * () { return std::move(itpair(idx, a->v[idx])); };
      const iter1 &operator ++() { idx++; return *this; }
      iter1 operator ++(int) { iter1 c(*this); idx++; return c; }
      bool operator ==(const iter1 &o) const { return idx == o.idx; }
      bool operator !=(const iter1 &o) const { return idx != o.idx; }

      const struct myvec *a;
      int idx, len;
    };

    iter1 begin() const { return iter1(a,idx,len); };
    iter1 end() const { return iter1(a,idx+len,0); };

  };

  typedef std::pair<int, range> rangepair;

  struct iter0 {

    typedef std::pair<int, range> rangepair;

    iter0(const myvec *a, int idx) : a(a), r(a->getrange(idx)) {};

    rangepair operator * () { return rangepair(r.idx, range(r)); };
    const iter0      &operator ++() { r = a->getrange(a->first_set(r.idx+r.len)); return *this; }
    iter0 operator ++(int) {
      iter0 c(*this); r = a->getrange(r.idx+r.len); return c;
    }
    bool operator ==(const iter0 &o) const {
      return r.idx == o.r.idx; }
    bool operator !=(const iter0 &o) const {
      return r.idx != o.r.idx; }

    const struct myvec *a;
    range r;
  };
  iter0 begin() const { return iter0(this,first_set()); };
  iter0 end()   const { return iter0(this,v.size()); };

  range getrange(int idx) const {
    int l = 0;
    for (l = 0; idx+l < v.size(); l++) {
      if (!v[idx+l])
	break;
    }
    return range(this, idx, l);
  }
  int first_set(int idx = 0) const {
    int i = 0;
    for (i = idx; i < v.size(); i++) {
      if (v[i])
	break;
    }
    return i;
  }

  std::vector<int> v;
};

int main(int argc, char **argv) {
  (void) argc; (void) argv;
  myvec v{1,2,3,0,40,41};

  for(const auto&& [i,r]: v) {
    std::cout << ":";
    for(const auto&& [j,v]: r) {
      std::cout << j << ",";
    }
    std::cout << std::endl;
  }

  /*for(const auto&& [i,r]: v.array) {
    }*/

  return 0;
}
