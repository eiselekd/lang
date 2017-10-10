%{
#define YYDEBUG 1
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "l4.h"

static int yylex();
static int yyerror();

struct _node * asttree = 0;

%}

%union {
    struct _node *node;
    int val;
}

%token <val>  INTEGER
%type <node>  addexpr numeric

%token PLUS 		/* unary+ */
%token MINUS 		/* unary- */

%left  '+' '-'

%%

program	 : addexpr              { printf("[=]\n"); asttree = $1; }

addexpr :  numeric '+' numeric  { printf("[+]\n");  }

numeric	 : INTEGER              { printf("[n] %d\n", $1);  }

%%

static char *lex_p = "1+2";

int
nextc()
{
  int c;
  c = *lex_p++;
  return c;
}

void
pushback(c)
    int c;
{
    if (c == -1) return;
    lex_p--;
}

static char *tokenbuf = NULL;
static int   tokidx, toksiz = 0;

#define tokfix() (tokenbuf[tokidx]='\0')
#define tok() tokenbuf
#define toklen() tokidx
#define toklast() (tokidx>0?tokenbuf[tokidx-1]:0)

char*
newtok()
{
    tokidx = 0;
    if (!tokenbuf) {
	toksiz = 60;
	tokenbuf = ALLOC_N(char, 60);
    }
    if (toksiz > 1024) {
	REALLOC_N(tokenbuf, char, 60);
    }
    return tokenbuf;
}

void
tokadd(c)
    char c;
{
    if (tokidx >= toksiz) {
	toksiz *= 2;
	REALLOC_N(tokenbuf, char, toksiz);
    }
    tokenbuf[tokidx++] = c;
}

static int
yyerror(char *msg)
{
  return 0;
}

static int
yylex()
{
  int c;
  newtok();

  switch (c = nextc()) {
  case '\0':		/* NUL */
  case -1:			/* end of script. */
    return 0;
  case '+':
    return '+';
  case '-':
    return '-';
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    {
      while (isdigit(c)) {
	tokadd(c);
	c = nextc();
      }
      pushback(c);
      tokfix();
      yylval.val = atoi(tok());
      return INTEGER;
    }
  }
  return 0;
}

struct _node*
yycompile(f)
    char *f;
{
    int n;
    n = yyparse();
    if (n == 0) return asttree;

    return 0;
}
