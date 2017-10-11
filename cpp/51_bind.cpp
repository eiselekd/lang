#include <map>
#include <iostream>
#include <type_traits>

class mypair {
public:
  mypair(int i0, int i1) :first(i0), second(i1) {};
  int first, second;
};

class a {
public:

  a() {};

  class iterator {
  public:
    mypair & operator *()  { return i_; }
    const iterator &operator ++() { ++i_.first; return *this; }
    iterator operator ++(int) { iterator copy(*this); ++i_.first; return copy; }

    bool operator ==(const iterator &other) const { return i_.first == other.i_.first; }
    bool operator !=(const iterator &other) const { return i_.first != other.i_.first; }

    iterator(int start) : i_{start,start} { }

    mypair i_;
   };

  iterator begin() const { return iterator(0); }
  iterator end() const { return iterator(10); }
};

template<std::size_t _Int>
struct __mypair_get;

template<>
struct __mypair_get<0>
{
  static constexpr int&
  __get(mypair& __pair) noexcept
  { return __pair.first; }
};

template<>
struct __mypair_get<1>
{
  static constexpr int&
  __get(mypair& __pair) noexcept
  { return __pair.second; }
};


// use std::tuple_element to generate first,second accessors
// structual binding will reference
// std::tuple_element<0, mypair>::type
// std::tuple_element<1, mypair>::type
template<std::size_t _Int>
constexpr typename std::tuple_element<_Int, mypair>::type&
get(mypair& __in) noexcept
{ return __mypair_get<_Int>::__get(__in); }

int main(int argc, char **argv) {
  a v0;
  for (auto & [i,j] : v0) {
    std::cout << i << "," << j << std::endl;
  }
  return 0;
}
