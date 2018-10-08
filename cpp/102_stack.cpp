#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>

/*
 * https://stackoverflow.com/questions/52681624/destructor-on-placement-new?noredirect=1#comment92291870_52681624
 */
template <typename T>
class stack_placement {
private:
    T *m_ptr;
public:
    template<typename... _Args> stack_placement(void *area, _Args&&... __args) {
	m_ptr = new(area) T(std::forward<_Args>(__args)...);
    }
    stack_placement(const stack_placement &) = delete;
    ~stack_placement() {
	m_ptr->~T();
    }
    void operator=(const stack_placement &) = delete;
    T *operator->() {
	return m_ptr;
    }
};

class aclass {
public:
    aclass(int size) : size_(size) { printf("aclass\n"); };
    ~aclass() {}
    int size_;
    char data[0];
};

void f(int size)
{
    intptr_t v0[(sizeof(aclass) + size + sizeof(intptr_t) - 1)/sizeof(intptr_t)];
    intptr_t v1[(sizeof(aclass) + size + sizeof(intptr_t) - 1)/sizeof(intptr_t)];
    aclass *p = new(static_cast<void*>(&v0)) aclass(size);
    p->~aclass();

    stack_placement<aclass> v(v1, size);

}

int main(int argc, char **argv)
{
    /*
      placement-new:
       http://www.scs.stanford.edu/~dm/home/papers/c++-new.html
       https://stackoverflow.com/questions/222557/what-uses-are-there-for-placement-new
    */
    f(10);
    f(100);
    return 0;
}
