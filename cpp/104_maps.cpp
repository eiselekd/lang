#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>
#include <new>
#include <map>
#include <type_traits>

using namespace std;

/*
  semimap:
  https://www.youtube.com/watch?v=qNAbGpV1ZkU
  https://github.com/hogliux/semimap/blob/master/semimap.h
*/
#include "semimap.h"

#define ID(x) []() constexpr { return x; }

template <auto...> struct d {};
template <typename l>
constexpr auto key2type(l l0) {
    return d<l0()>{};
}

template <typename Key>
class m {
public:
    m() = delete;
    template <typename id,typename ... Args>
    static int get(id i, Args&&... args)
    {
	using u = decltype(key2type(i));
        auto& f = init_flag<u>;
	if (f) {
	}
	return 0;
    }
    template <typename>
    static bool init_flag;
};

template <typename Key>
template <typename>
bool m<Key>::init_flag = false;

int main(int argc, char **argv)
{

    m<int>::get(ID(1));
    m<int>::get(ID(2));

    //vector<decltype(key2type(ID(1)))> a;



    {
	semi::map<std::string, std::string> map;

	auto& food = map.get(ID("food"));
	assert(food.empty());
	food = "pizza";
	assert(map.get(ID("food")) == "pizza");
    };
    {

	struct Tag {
        };
        using map = semi::static_map<std::string, std::string, Tag>;
        // compile-time first, then run-time
        map::get(ID("food")) = "pizza";
        assert(map::get("food") == "pizza");
    };

    return 0;
}
