/* Local Variables: */
/* buffer-gtest-rule:"gtest-lexer" */
/* End: */

/* apt-get install libgtest-dev g++-7 */
/*
[a-zA-Z0-9_]+    : symbol
[0-9]+           : numbers
".."             : string
+,-,*            : arith
{,},(,),[,]      : structs
fun, if, for     : keywords
*/

#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include <ctype.h>
#include "./token.h"
#include <vector>
#include <string>

using namespace testing;
using namespace std;

struct str {
    str(const char *p, int len) : p(p), len(len) { }
    const char *p;
    int len;
};

bool operator == (const struct str &a, const struct str &b) {
    return a.len == b.len &&
	strncmp(a.p, b.p, a.len) == 0;
};

struct tok {
    int tag;
    str tok;
    string as_string() const { return string(tok.p,tok.len); }
};

bool operator == (const struct tok &a, const struct tok &b) {
    return a.tag == b.tag &&
	a.tok == b.tok;
}

std::ostream& operator<<(std::ostream& os, const tok &e) {
    return os << e.tag << ":'" << e.as_string() << "'";
}

tok
lex(const char **_p, int len)
{
    int i = 0, prev, c0, c1;
    int tag = 0; int tokstart = 0;
    const char *p = *_p;

restart:
    switch(c0 = p[i]) {
    case ' ':
	tokstart = ++i;
	goto restart;
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
	tag = TOK_STR; break;
    }
    case '0'...'9':
	while (i < len && isdigit(p[i]))
	    i++;
	tag = TOK_INT; break;
    };
    *_p = p+i;
    return tok{tag,{p+tokstart,i-tokstart}};
}

vector<tok>
lex_all(const char *p) {
    vector<tok> v; int l = strlen(p);
    const char *e = p + l;
    while(e > p) {
	tok i = lex(&p, e-p);
	v.push_back(i);
    }
    return v;
}

TEST(Lexing, Rawtokens)
{
    EXPECT_THAT(lex_all("1 2"),ElementsAreArray(
		    {tok{TOK_INT,{"1",1}},
		     tok{TOK_INT,{"2",1}}}));
    EXPECT_THAT(lex_all("\"str\""),ElementsAreArray(
		    {tok{TOK_STR,{"\"str\"",5}}}));

}

int lexer_main(int argc, char **argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
