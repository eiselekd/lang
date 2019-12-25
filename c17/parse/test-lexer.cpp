/*
 * Local Variables:
 * buffer-gtest-rule:"gtest-lexer"
 * End:
 */

#include "gtest/gtest.h"

extern int lexer_main(int argc, char **argv);
extern int flex_lexer_main(int argc, char **argv);



int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
