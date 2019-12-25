/* Local Variables: */
/* buffer-gtest-rule:"gtest-lexer" */
/* End: */

%{

int yyerror(const char *s);
int yylex(void);

%}

%union{
  int		int_val;
  char*	        op_val;
}

%start	start

%token TOK_IF TOK_STR TOK_THEN TOK_ELSE
%token TOK_INT

%%

start:

%%

int yyerror(const char *s)
{
	return 0;
}
