#include <map>
#include <iostream>

class a {
public:

  a() {};

  class iterator {
  public:
    int (& operator *()) [2] { return i_; }
    const iterator &operator ++() { ++i_[0]; return *this; }
    iterator operator ++(int) { iterator copy(*this); ++i_[0]; return copy; }

    bool operator ==(const iterator &other) const { return i_[0] == other.i_[0]; }
    bool operator !=(const iterator &other) const { return i_[0] != other.i_[0]; }

    iterator(int start) : i_{start,start} { }

    int i_[2];
   };

  iterator begin() const { return iterator(0); }
  iterator end() const { return iterator(10); }

};

int main(int argc, char **argv) {
  a v0;
  for (auto & [i,j] : v0) {
    std::cout << i << "," << j << std::endl;
  }
  return 0;
}
