#include <stdio.h>
#include <stdlib.h>

class CId {
};

class h {
public:
  class ref {
  public:

    h *_h;
    int _i;

    ref& operator=(int i) { return *this; };

    operator CId* () const { return 0; };

  };

  ref operator[](int i) { return ref{this,i}; };
};

int main(int argc, char **argv) {
  h a;
  CId *b;

  b = a[1];
  return 0;

}
