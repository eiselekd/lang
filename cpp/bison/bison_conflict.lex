/* Mini Calculator */
/* calc.lex */

%{
#include "heading.h"
#include "bison_conflict_no.tab.h"
int yyerror(char *s);
//int yylineno = 1;


%}



digit		[0-9]
int_const	{digit}+

%%

{int_const}	{ yylval.int_val = atoi(yytext); return INTEGER_LITERAL; }
"+"		{ yylval.op_val = new std::string(yytext); return PLUS; }
"*"		{ yylval.op_val = new std::string(yytext); return MULT; }

"if"		{ return TOK_IF; }
"then"		{ return TOK_THEN; }
"else"		{ return TOK_ELSE; }


[ \t]*		{}
[\n]		{ yylineno++;	}

.		{ std::cerr << "SCANNER "; yyerror(""); exit(1);	}


