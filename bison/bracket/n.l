%option noyywrap nounput noinput batch debug
%x l8 retnl

%{

#include "parse.h"

%}


id    [a-zA-Z][a-zA-Z_0-9]*
int   [0-9]+
blank [ \t\r]

%%

<INITIAL>.|\n 		{BEGIN l8; yyless(0); }

<retnl>[\n]           {return '\n';}
<l8>[\n]              { }
<l8,retnl>[ \t]       { }
<l8,retnl>[#][^\n]*   { }

<l8,retnl>fun         { return FUNC;   }

<l8,retnl>"{"		{return '{';}
<l8,retnl>"}"		{return '}';}
<l8,retnl>"("		{return '(';}
<l8,retnl>")"		{return ')';}
<l8,retnl>"+"		{return '+';}
<l8,retnl>";"		{return ';';}

<l8,retnl>{id}        {return ID; }
<l8,retnl>{int}       {return NUM; }

%%
