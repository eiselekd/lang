%{ /* -*- C++ -*- */
# include <cerrno>
# include <climits>
# include <cstdlib>
# include <cstring> // strerror
# include <string>
# include "driver.hh"
# include "parser.hpp"

// Work around an incompatibility in flex (at least versions
// 2.5.31 through 2.5.33): it generates code that does
// not conform to C89.  See Debian bug 333231
// <http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=333231>.
# undef yywrap
# define yywrap() 1

%}
%option noyywrap nounput noinput batch debug

%{
  // A number symbol corresponding to the value in S.
    yy::parser::symbol_type
      make_NUMBER (const std::string &s, const yy::parser::location_type& loc);
%}

id    [a-zA-Z][a-zA-Z_0-9]*
int   [0-9]+
blank [ \t\r]



%{
	// Code run each time a pattern is matched.
# define YY_USER_ACTION  loc.columns (yyleng);
%}

%%

%{
	// A handy shortcut to the location held by the driver.
	yy::location& loc = drv.location;
	// Code run each time yylex is called.
	loc.step ();
%}

{blank}+   loc.step ();
[\n]+      loc.lines (yyleng); loc.step ();

"-"        return yy::parser::make_MINUS  (loc);
"+"        return yy::parser::make_PLUS   (loc);
"*"        return yy::parser::make_STAR   (loc);
"/"        return yy::parser::make_SLASH  (loc);
"("        return yy::parser::make_LPAREN (loc);
")"        return yy::parser::make_RPAREN (loc);
":="       return yy::parser::make_ASSIGN (loc);

{int}      return make_NUMBER (yytext, loc);
{id}       return yy::parser::make_IDENTIFIER (yytext, loc);

.          {
	throw yy::parser::syntax_error
		(loc, "invalid character: " + std::string(yytext));
	   }

<<EOF>>    return yy::parser::make_YYEOF (loc);

%%

yy::parser::symbol_type
make_NUMBER (const std::string &s, const yy::parser::location_type& loc)
{
	errno = 0;
	long n = strtol (s.c_str(), NULL, 10);
	if (! (INT_MIN <= n && n <= INT_MAX && errno != ERANGE))
		throw yy::parser::syntax_error (loc, "integer is out of range: " + s);
	return yy::parser::make_NUMBER ((int) n, loc);
}

void
driver::scan_begin ()
{
  yy_flex_debug = trace_scanning;
  if (file.empty () || file == "-")
    yyin = stdin;
  else if (!(yyin = fopen (file.c_str (), "r")))
    {
      std::cerr << "cannot open " << file << ": " << strerror (errno) << '\n';
      exit (EXIT_FAILURE);
    }
}

void
driver::scan_end ()
{
  fclose (yyin);
}
