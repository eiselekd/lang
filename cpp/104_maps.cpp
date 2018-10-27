#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>
#include <new>

/*
  semimap:
  https://www.youtube.com/watch?v=qNAbGpV1ZkU
  https://github.com/hogliux/semimap/blob/master/semimap.h
*/
#include "semimap.h"

#define ID(x) \
    []() constexpr { return x; }

/*
   std::launder: volatile like instruction used to prevent compile time aliasing
   http://code.i-harness.com/ja/docs/cpp/utility/launder


 */

int main(int argc, char **argv)
{

    struct X {
	const int n; // note: X has a const member
	int m;
    };
    {
	X *p = new X{3};
	const int a = p->n;
	new (p) X{5};       // p does not point to new object because X::n is const
	const int b = p->n; // undefined behavior
	const int x = p->m; // undefined behavior (even though m is non-const, p can't be used)
	const int c = std::launder(p)->n; // OK, std::launder(p) points to new object
    }

    struct {
    X *p = new X{3, 4};
    const int a = p->n;
    X* np = new (p) X{5, 6};

    {
	semi::map<std::string, std::string> map;

	auto& food = map.get(ID("food"));
	assert(food.empty());
	food = "pizza";
	assert(map.get(ID("food")) == "pizza");
    }
    {

	struct Tag {
        };
        using map = semi::static_map<std::string, std::string, Tag>;
        // compile-time first, then run-time
        map::get(ID("food")) = "pizza";
        assert(map::get("food") == "pizza");
    }

    return 0;
}
