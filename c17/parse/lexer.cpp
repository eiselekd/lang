/*
 * Local Variables:
 * buffer-gtest-rule:"gtest-lexer"
 * End:
 */

/*
[a-zA-Z0-9_]+    : symbol
[0-9]+           : numbers
".."             : string
+,-,*            : arith
{,},(,),[,]      : structs
fun, if, for     : keywords
*/

#include "gtest/gtest.h"
#include <ctype.h>

int
lex(const char *p, int len)
{
    int i = 0, j = 0, prev, c0, c1;
    switch(c0 = p[i]) {
    case 'a'...'z':
    case 'A'...'Z':
	break;
    case '"': {
	i++; prev=0;
	while ((i < len) &&
	       ((c0 != (c1=p[i++])) || (prev == '\\')))
	{
	    prev = (prev == '\\' && c1 == '\\') ? 0 : c1;
	}
	break;
    }
    case '0'...'9':
	while (i < len && isdigit(p[i]))
	    i++;
	break;
    };
    return 0;
}

TEST(Lexing, Rawtokens) {
    EXPECT_EQ(1, lex("1",1));
    EXPECT_EQ(1, lex("1",1));
}

int lexer_main(int argc, char **argv) {
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
