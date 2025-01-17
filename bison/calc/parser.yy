%skeleton "lalr1.cc" // -*- C++ -*-
%require "3.8.1"
%header
%define api.token.raw
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%code requires {
# include <string>
class driver;
}

// The parsing context.
%param { driver& drv }
%locations

%define parse.trace
%define parse.error detailed
%define parse.lac full
%code {
	# include "driver.hh"
}

%define api.token.prefix {TOK_}
%token
  ASSIGN  ":="
  MINUS   "-"
  PLUS    "+"
  STAR    "*"
  SLASH   "/"
  LPAREN  "("
  RPAREN  ")"
;
%token <std::string> IDENTIFIER "identifier"
%token <int> NUMBER "number"
%nterm <int> exp
%printer { yyo << $$; } <*>;

%%
%start unit;
unit: assignments exp  { drv.result = $2; };

assignments:
%empty                 {}
| assignments assignment {};

assignment:
"identifier" ":=" exp { drv.variables[$1] = $3; };

%left "+" "-";
%left "*" "/";
exp:
  "number"
  | "identifier"  { $$ = drv.variables[$1]; }
| exp "+" exp   { $$ = $1 + $3; }
| exp "-" exp   { $$ = $1 - $3; }
| exp "*" exp   { $$ = $1 * $3; }
| exp "/" exp   { $$ = $1 / $3; }
| "(" exp ")"   { $$ = $2; }
%%
void
yy::parser::error (const location_type& l, const std::string& m)
{
	std::cerr << l << ": " << m << '\n';
}
