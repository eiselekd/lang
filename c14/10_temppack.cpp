#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <functional>

template<typename T> struct tag {};

template<typename TFunctionObject, typename TFirstParamType>
struct Binder {
  TFunctionObject function;
  TFirstParamType param;
  template<typename... TParams>
  auto operator()(TParams&&... params) -> decltype(function(param, std::forward<TParams>(params)...)) {
    return function(param, std::forward<TParams>(params)...);
  }
};

class A;
typedef void (*fn_t) (A*);

class A {

public:
  int a[3];
  void *f;
  fn_t callback;

  template<typename TCallback>
  static int readIntoFunction(TCallback&& callback, A *c, int ) {
    return callback();
  }
  template<typename TCallback, typename TFirstType, typename... TTypes>
  static int readIntoFunction(TCallback&& callback, A *c, int index, tag<TFirstType>, tag<TTypes>... othersTags)
  {
    const TFirstType v = c->a[index];
    Binder<TCallback, const TFirstType&> binder{ callback, v };
    return readIntoFunction(binder, c, index + 1, othersTags...);
  }

  template<typename TType, typename = void>
  struct Split {
    template<typename TType2>
    static void push(A *c, TType2 value) {
    }
  };

  void call() {
    callback(this);
  }

  template<typename TReturnType, typename... TParameters>
  struct Split<TReturnType (TParameters...)>
  {
    template<typename TFunctionObject>
    static void push(A *c, TFunctionObject value) noexcept {

      //printf("Sizeof %d %p\n", (int)sizeof(TFunctionObject), value);

      c->f = (void *)static_cast<TFunctionObject*>(malloc(sizeof(TFunctionObject)));
      new (c->f) TFunctionObject(std::move(value));

      const auto g = ([](A *c) -> void {
	  auto value = static_cast<TFunctionObject*>(c->f);
	  A::readIntoFunction(*value, c, 0, tag<TParameters>{}...);
	});
      c->callback = g;
    }
  };

  template<typename TReturnType, typename... TParameters>
  struct Split<TReturnType (*)(TParameters...)>
  {
    typedef Split<TReturnType(TParameters...)> SubPusher;
    template<typename TType>
    static void push(A *c, TType value) noexcept {
      SubPusher::push(c, value);
    }
  };

  template<typename TFunctionType>
  auto ca(TFunctionType&& data)  {
    typedef typename std::decay<TFunctionType>::type RealDataType;
    Split<RealDataType>::push(this, std::forward<TFunctionType>(data));
  }
};

int main(int argc, char **argv) {

  A c{{ 1, 2, 3}};
  c.ca( (int (*)(int,float))[](int a,float b)->int {
      std::cout << a << b << std::endl;
    });
  c.call();

  c.ca( (int (*)(int,float,double))[](int a,float b,double c)->int {
      std::cout << a << b << c << std::endl;
    });
  c.call();


}
