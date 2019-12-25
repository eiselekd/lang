%{
int yyerror(const char *s);
int yylex(void);
%}

%start	start

%%

start:

%%

int yyerror(const char *s)
{
	return 0;
}
