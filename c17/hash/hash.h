#ifndef DEF_HASH_H
#define DEF_HASH_H

#include "tripple.h"

template<typename _TypeKey=int, typename _TypeVal=int>
struct HashNode {
    struct HashNode *n;
    _TypeKey key;
    _TypeVal val;
};

template<typename _TypeKey=int, typename _TypeVal=int, typename _Alloc = std::allocator<struct HashNode<_TypeKey, _TypeVal>>>
class CHash
{
    typedef struct HashNode<_TypeKey, _TypeVal> _HashNode;

    class _hashvec
    {
	typedef std::vector<_HashNode, _Alloc> _Base;

	typedef typename __gnu_cxx::__alloc_traits<_Alloc>::template rebind<_HashNode>::other _Tp_alloc_type;
	typedef __gnu_cxx::__alloc_traits<_Tp_alloc_type> _Alloc_traits;

    public:
	_hashvec(int order) noexcept : order(order), cnt(0), elems((1<<order))
	{ }

	_hashvec( _hashvec && __x) noexcept :
	order(__x.order), cnt(__x.cnt), elems(std::move(__x.elems))
	{ }

	_hashvec&
	operator=(_hashvec&& __x) noexcept(_Alloc_traits::_S_nothrow_move())
	{
	    order = __x.order;
	    cnt = __x.cnt;
	    elems = std::move(__x.elems);
	    return *this;
	}

    protected:
	size_t order;
	size_t cnt;
	_Base elems;
    };

};

#endif
