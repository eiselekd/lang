/* Mini Calculator */
/* calc.y */

%{
#include "heading.h"
int yyerror(char *s);
int yylex(void);
%}

%union{
  int		int_val;
  string*	op_val;
}

%start	Statement
 // input

%token TOK_IF TOK_THEN TOK_ELSE

%token	<int_val>	INTEGER_LITERAL
%left	PLUS
%left	MULT



%%
// http://www.goldparser.org/doc/grammars/example-if-then-else.htm

Statement : TOK_IF INTEGER_LITERAL TOK_THEN Statement
          | TOK_IF INTEGER_LITERAL TOK_THEN Statement_THEN  TOK_ELSE Statement
          | INTEGER_LITERAL
;


Statement_THEN :
          | TOK_IF INTEGER_LITERAL TOK_THEN Statement_THEN TOK_ELSE Statement_THEN
          | INTEGER_LITERAL
;




%%

int yyerror(string s)
{
  extern int yylineno;	// defined and maintained in lex.c
  extern char *yytext;	// defined and maintained in lex.c

  cerr << "ERROR: " << s << " at symbol \"" << yytext;
  cerr << "\" on line " << yylineno << endl;
  exit(1);
}

int yyerror(char *s)
{
  return yyerror(string(s));
}

/* main.cc */

#include "heading.h"

// prototype of bison-generated parser function
int yyparse();

int main(int argc, char **argv)
{
  if ((argc > 1) && (freopen(argv[1], "r", stdin) == NULL))
  {
    cerr << argv[0] << ": File " << argv[1] << " cannot be opened.\n";
    exit( 1 );
  }

  yyparse();

  return 0;
}
