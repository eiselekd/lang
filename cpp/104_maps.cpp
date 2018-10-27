#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unistd.h>
#include <assert.h>

/*
  semimap:
  https://www.youtube.com/watch?v=qNAbGpV1ZkU
  https://github.com/hogliux/semimap/blob/master/semimap.h
*/
#include "semimap.h"

#define ID(x) \
    []() constexpr { return x; }

int main(int argc, char **argv) {
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
