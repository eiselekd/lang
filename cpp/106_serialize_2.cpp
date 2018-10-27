#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>
#include <new>
#include <new>
#include <cstddef>
#include <cassert>
#include <type_traits>
#include <vector>
#include <functional>

/* https://www.youtube.com/watch?v=dV0FTkEl0W4 */

using namespace std;
using SerFunc = std::function<void(const void *,vector<char> &)>;
using SerFuncs = vector<SerFunc>;

struct f {
    int a, b;
    vector<int> v_;
};

struct g {
    f n;
    int b;
    vector<int> v_;
};

template <typename n> SerFuncs& EnumSerFuncs();

template<typename e>
void EnumSerCall(SerFuncs &fa, e &v, std::vector<char> &out)
{
    const void *p = static_cast<const void*>(&v);
    for (auto f : fa) {
	f(p,out);
    }
}

template<typename e>
void EnumSer(const e &v, std::vector<char> &out)
{
    if constexpr (std::is_trivially_copyable_v<e>) {
	    auto o = out.size();
	    auto n = o + sizeof(e);
	    out.resize(n);
	    *(reinterpret_cast<e*>(out.data() + o)) = v;
	    printf(".");
    } else {
	printf(".");
	EnumSerCall(EnumSerFuncs<e>(),v,out);
    }
}

template <typename mt, typename d, d (mt::* p)>
auto EnumSerOff(const void *v,vector<char> &out) -> void {
    auto m = reinterpret_cast<const mt*>(v);
    printf(",");
    EnumSer(m->*p,out);
}

template<typename n> SerFuncs& EnumSerFuncs();

/* template specialization: */
#define RSC_DATA_BEG(f)				\
    template<>					\
    SerFuncs& EnumSerFuncs<f>() {		\
    static SerFuncs f_ = {
#define RSC_DATA(f,v) EnumSerOff<f,decltype(f::v), &f::v>,
#define RSC_DATA_END(n) }; return f_; }

RSC_DATA_BEG(f)
RSC_DATA(f,a)
RSC_DATA_END(f)

RSC_DATA_BEG(g)
RSC_DATA(g,n)
RSC_DATA_END(g)

/*
template <typename n>
SerFuncs& EnumSerFuncs() {
    static SerFuncs f = {
	&EnumSerOff<int,offsetof(n,a)>,
	&EnumSerOff<int,offsetof(n,b)>,
    };
    return f;
    }*/


int main(int argc, char **argv) {
    g g0{{1,2},3};
    std::vector<char> v;
    EnumSer(g0,v);
}
