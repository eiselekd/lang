#include <stdio.h>
#include <iostream>
#include <vector>
#include <map>

template<typename _T1, typename _T2, typename _T3>
struct tripple
{
  _T1 v0;
  _T2 v1;
  _T2 v2;
};

/* c++17 structural binding support for tripples: specialize tupple accessors*/
/* predicate: */
template<typename _T1, typename _T2, typename _T3 > struct std::__is_tuple_like_impl<tripple<_T1, _T2, _T3>> : true_type { };
/* size: */
template<typename _T1, typename _T2, typename _T3 > struct std::tuple_size<tripple<_T1, _T2, _T3>> : public std::integral_constant<std::size_t, 3> { };
/* per element type: */
template<typename _T1, typename _T2, typename _T3 > struct std::tuple_element<0,tripple<_T1, _T2, _T3>> { typedef _T1 type; };
template<typename _T1, typename _T2, typename _T3 > struct std::tuple_element<1,tripple<_T1, _T2, _T3>> { typedef _T2 type; };
template<typename _T1, typename _T2, typename _T3 > struct std::tuple_element<2,tripple<_T1, _T2, _T3>> { typedef _T3 type; };
/* per element get: */
template<std::size_t _Int> struct __tripple_get;
template<> struct __tripple_get<0> {
    template<typename _T1, typename _T2, typename _T3 > static constexpr       _T1  & __get       (      tripple<_T1, _T2, _T3>& __p) noexcept { return                   __p.v0; }
    template<typename _T1, typename _T2, typename _T3 > static constexpr       _T1  &&__get_move  (      tripple<_T1, _T2, _T3>&&__p) noexcept { return std::forward<_T1>(__p.v0); }
    template<typename _T1, typename _T2, typename _T3 > static constexpr const _T1  & __get_const (const tripple<_T1, _T2, _T3>& __p) noexcept { return                   __p.v0; } };
template<> struct __tripple_get<1> {
    template<typename _T1, typename _T2, typename _T3 > static constexpr       _T2  & __get       (      tripple<_T1, _T2, _T3>& __p) noexcept { return                   __p.v1; }
    template<typename _T1, typename _T2, typename _T3 > static constexpr       _T2  &&__get_move  (      tripple<_T1, _T2, _T3>&&__p) noexcept { return std::forward<_T2>(__p.v1); }
    template<typename _T1, typename _T2, typename _T3 > static constexpr const _T2  & __get_const (const tripple<_T1, _T2, _T3>& __p) noexcept { return                   __p.v1; } };
template<> struct __tripple_get<2> {
    template<typename _T1, typename _T2, typename _T3 > static constexpr       _T3  & __get       (      tripple<_T1, _T2, _T3>& __p) noexcept { return                   __p.v2; }
    template<typename _T1, typename _T2, typename _T3 > static constexpr       _T3  &&__get_move  (      tripple<_T1, _T2, _T3>&&__p) noexcept { return std::forward<_T3>(__p.v2); }
    template<typename _T1, typename _T2, typename _T3 > static constexpr const _T3  & __get_const (const tripple<_T1, _T2, _T3>& __p) noexcept { return                   __p.v2; } };

template<std::size_t _Int, typename _T1, typename _T2, typename _T3>       constexpr typename std::tuple_element<_Int, tripple<_T1, _T2, _T3>>::type& get(      tripple<_T1, _T2, _T3>&  __in) noexcept { return __tripple_get<_Int>::__get      (          __in);  }
template<std::size_t _Int, typename _T1, typename _T2, typename _T3>       constexpr typename std::tuple_element<_Int, tripple<_T1, _T2, _T3>>::type&&get(      tripple<_T1, _T2, _T3>&& __in) noexcept { return __tripple_get<_Int>::__get_move (std::move(__in)); }
template<std::size_t _Int, typename _T1, typename _T2, typename _T3> const constexpr typename std::tuple_element<_Int, tripple<_T1, _T2, _T3>>::type& get(const tripple<_T1, _T2, _T3>&  __in) noexcept { return __tripple_get<_Int>::__get_const(          __in);  }

int main(int argc, char **argv) {
  (void) argc; (void) argv;
  typedef tripple<int, int, int> typ;
  std::vector<typ> v0{{0,1,2},{3,4,5}};

  for (auto &&[a,b,c]: v0) {
    (void) a; (void) b; (void) c;
    std::cout << a << "," << b << "," << c <<  std::endl;
  }

  return 0;
}
