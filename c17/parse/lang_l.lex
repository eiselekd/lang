/* Local Variables: */
/* buffer-gtest-rule:"gtest-lexer" */
/* End: */

%{
#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "./lang_y.hpp"
#include <vector>

using namespace testing;
%}

%option noyywrap

digit  [0-9]

%%

"part" { return TOK_STR; }
"fn"   { return TOK_INT; }

%%

std::vector<int> prep_yy_lex(const char *v) {
    int i; std::vector<int> vec;
    yy_scan_string(v);
    while ( (i = yylex())) {
	vec.push_back(i);
    }
    return vec;
}

TEST(FlexLexing, Lextokens)
{
    EXPECT_THAT(prep_yy_lex("part"),    ElementsAreArray({TOK_STR}));
    EXPECT_THAT(prep_yy_lex("part fn"), ElementsAreArray({TOK_STR, TOK_INT}));
}

int flex_lexer_main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
