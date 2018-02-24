// https://www.codeproject.com/Articles/1089905/A-Custom-STL-std-allocator-Replacement-Improves-Pe

#include <list>

#include <string>
#include <stdlib.h>

template <typename T>
class stl_allocator
{
public:
    typedef size_t size_type;
    typedef ptrdiff_t difference_type;
    typedef T* pointer;
    typedef const T* const_pointer;
    typedef T& reference;
    typedef const T& const_reference;
    typedef T value_type;

    stl_allocator(){}
    ~stl_allocator(){}

    template <class U> struct rebind { typedef stl_allocator<U> other; };
    template <class U> stl_allocator(const stl_allocator<U>&){}

    pointer address(reference x) const {return &x;}
    const_pointer address(const_reference x) const {return &x;}
    size_type max_size() const throw() {return size_t(-1) / sizeof(value_type);}

    pointer allocate(size_type n)
    {
        return static_cast<pointer>(malloc(n*sizeof(T)));
    }

    void deallocate(pointer p, size_type n)
    {
        free(p);
    }

    void construct(pointer p, const T& val)
    {
        new(static_cast<void*>(p)) T(val);
    }

    void construct(pointer p)
    {
        new(static_cast<void*>(p)) T();
    }

    void destroy(pointer p)
    {
        p->~T();
    }
};


#include <list>

template<class _Ty, class _Ax = stl_allocator<_Ty> >
class xlist : public std::list<_Ty, _Ax>
{

};

int main(int argc, char **argv) {
    xlist<std::string> myList;
    myList.push_back("hello world");
    return 0;
}
