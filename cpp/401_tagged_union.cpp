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

// Macro to obtain a numbered macro for argument counts
// - Raw variant
#define TU_GM_I(SUF,_1,_2,_3,_4,_5,_6,_7,_8,_9,_10,_11,_12,_13,_14,_15,_16,COUNT,...) SUF##COUNT
#define TU_GM(SUF,...) TU_EXP1( TU_GM_I(SUF, __VA_ARGS__,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0) )

#define ARGS(...)  TU_GM(,__VA_ARGS__)

    std::cout << "Count macro args:" << ARGS(a1,a2,a3) << std::endl;
    std::cout << "Count macro args:" << ARGS(a1) << std::endl;

    return 0;
}
