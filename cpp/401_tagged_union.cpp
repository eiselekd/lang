#include "tagged_union.hpp"
#include <iostream>
#include <vector>
#include <string>

// from mrustc:
// Compile-time known values
TAGGED_UNION_EX(Constant, (), Int, (
    (Int, struct {
        ::std::int64_t  v;
        }),
    (Uint, struct {
        ::std::uint64_t v;
        }),
    (Float, struct {
        double  v;
        }),
    (Bool, struct {
        bool    v;  // NOTE: Defensive to prevent implicit casts
        }),
    (Bytes, ::std::vector< ::std::uint8_t>),    // Byte string
    (StaticString, ::std::string)  // String
    ), (), (), (
    )
);

int main(int argc, char** argv) {
    std::vector<Constant> v;
    v.push_back(Constant::make_Int({1}));
    v.push_back(Constant::make_Float({2.0}));

    for (auto &v: v) {
	TU_MATCH(Constant, (v), (e),
		 (Int, std::cout << "Int:" << e.v << std::endl;),
		 (Float, std::cout << "Float" << e.v << std::endl;)
	    );
    }
    return 0;
}
