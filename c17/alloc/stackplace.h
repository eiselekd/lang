#ifndef L8_H_STACKPLACE_
#define L8_H_STACKPLACE_
#include <utility>

/* automaticaly call destructor on new-placement on stack object:
 * char v[100];
 * new((void*)v) obj(arg0);
 * convert to:
 * stk<obj> o(v, arg0);
 */
template <typename T>
class stk {
private:
    T *m_ptr;
public:
    template<typename... _Args> stk(void *area, _Args&&... __args) {
	m_ptr = new(area) T(std::forward<_Args>(__args)...);
    }
    stk(void *area, int size) {
	m_ptr = new(area) T(size);
    }
    stk(const stk &) = delete;
    ~stk() {
	m_ptr->~T();
    }
    void operator=(const stk &) = delete;
    T *operator->() {
	return m_ptr;
    }
};

#endif
