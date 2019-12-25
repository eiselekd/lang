/* Local Variables: */
/* buffer-gtest-rule:"gtest-lexer" */
/* End: */

%{
#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include "./token.h"
#include <vector>
%}

%option noyywrap

digit         [0-9]

%%

"part" { return TOK_STR; }
"fn"   { return TOK_INT; }

%%

int prep_yy_lex(const char *v) {
    yy_scan_string(v);
    return yylex();
}

TEST(FlexLexing, Lextokens)
{
    EXPECT_EQ(TOK_STR, prep_yy_lex("part fn"));

    std::vector v{1,2,3};
    EXPECT_THAT(v, ::testing::ElementsAreArray({1,2,3}));

    EXPECT_EQ(TOK_INT, prep_yy_lex("fn"));
}

int flex_lexer_main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
