
%{
#define YYDEBUG 1

%}

%code requires {


extern int yy_start;
#define retnl 2

extern enum yytokentype yylex();
  extern void yyerror(const char* errmsg);
  extern void yyerrorf(const char* format, ...);

}

%expect 0
//			%define api.pure
//			%locations
%define parse.trace
%verbose
%header
%define parse.error verbose


%token  FUNC

%token  ID NUM

%left  '+'

%%
%start unit;

unit: stmts

stmts:
		stmt                       {}
	|	stmts  stmt                {}

stmt:
		expr D                     { yy_start = 1 + 2 * 1 /*state:l8*/; }
	;

D:		';'
	|	'\n'
	;


expr:		expr '+' expr              {}
	|	primary                    {}
	;

primary:
		NUM                        {}
	|	ID                         {}
	|	FUNC     '(' ')' '{' stmts { yy_start = 1 + 2 * 2 /*state:retnl*/; } '}' { }
	|	FUNC ID '('  ')' '{' stmts { yy_start = 1 + 2 * 2 /*state:retnl*/; } '}' {}
	;

%%

void yyerror(const char* errmsg)
{
  printf("%s",errmsg);
}
