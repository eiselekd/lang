#ifndef _ALLOC_STL_HEADER_
#define _ALLOC_STL_HEADER_

#include <stdlib.h>
#include <vector>

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

    stl_allocator() {}
    ~stl_allocator() {}

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

#if __cplusplus >= 201103L

    template<typename _Up, typename... _Args>
    void
    construct(_Up* __p, _Args&&... __args)
    { ::new((void *)__p) _Up(std::forward<_Args>(__args)...); }

    template<typename _Up>
    void
    destroy(_Up* __p) { __p->~_Up(); }

#endif

    void construct(pointer p)
    {
        new(static_cast<void*>(p)) T();
    }

    void destroy(pointer p)
    {
        p->~T();
    }

    pointer newExtra(size_type extra)
    {
        return static_cast<pointer>(malloc(extra+sizeof(T)));
    }

    template<typename... _Args>
    pointer
    newObj(size_type size, _Args&&... __args)
    {
	pointer p = newExtra(size);
	::new((void*) p) T(std::forward<_Args>(__args)...); return p; }


    void relObj(pointer p)
    {
	destroy(p);
        free(p);
    }

};

#endif
