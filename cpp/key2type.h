#define ID(x) []() constexpr { return x; }
template <auto...> struct d {};
template <typename l>
constexpr auto key2type(l l0) {
    return d<l0()>{};
}
