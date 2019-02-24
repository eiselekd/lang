
%x incl
%x find
%x vhdl

%{

#define PLUS 1
#define MINUS 2
#define NUMBER 3
#define UNKNOWN 1

typedef union _yylval_t {
    int num;
} yylval_t;

/*
#define YY_INPUT(buf,result,max_size)				\
    {								\
	int c = getc(yyin);					\
	result = (c == EOF) ? YY_NULL : (buf[0] = c, 1);	\
    }
*/

#define MAX_INCLUDE_DEPTH 10
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int include_stack_ptr = 0;

#define YY_DECL int yylex (yylval_t *yylval, yyscan_t yyscanner)

%}

%option reentrant
%option noyywrap




digit         [0-9]



%%


"+" { return PLUS;       }
"-" { return MINUS;      }
{digit}+  {
    yylval->num = atoi(yytext);
    return NUMBER;     }
[ \t\n\r]            /* skip whitespace */

.                    {
    printf("Unknown character [%c]\n",yytext[0]);
    return UNKNOWN;    }


vhdl BEGIN(vhdl);


inc BEGIN(incl);
<incl>[ \t]*      /* eat the whitespace */
<incl>[^ \t\n]+   { /* got the include file name */

    if ( include_stack_ptr >= MAX_INCLUDE_DEPTH )
    {
	fprintf( stderr, "Includes nested too deeply" );
	exit( 1 );
    }
    {
	FILE *f;
	f = fopen( yytext, "r" );
	if ( ! f ) {
	    printf( "cannot find %s" , yytext );
	    //BEGIN(INITIAL);
	    break;
	}
	yyin = f;
    }
    include_stack[include_stack_ptr++] = YY_CURRENT_BUFFER;

    yy_switch_to_buffer(
	yy_create_buffer( yyin, YY_BUF_SIZE, yyscanner ), yyscanner );

    BEGIN(INITIAL);
 }

<<EOF>> {
    if ( --include_stack_ptr < 0 )
    {
	yyterminate();
    }

    else
    {
	yy_delete_buffer( YY_CURRENT_BUFFER, yyscanner );
	yy_switch_to_buffer(
	    include_stack[include_stack_ptr], yyscanner );
    }
  }

%%

typedef struct scan {
    struct yyguts_t g;
} scan_t;

int
main( int argc, char **argv )
{
    int c;
    scan_t scan;
    yylval_t v;
    memset(&scan, 0, sizeof(scan));
    scan.g.yyin_r = stdin;

    while (c = yylex(&v, &scan)) {
	switch(c) {
	case NUMBER:
	    printf("Number %d\n", v.num);
	}
    }
}
