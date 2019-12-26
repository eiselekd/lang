%{
%}

%option noyywrap
digit  [0-9]

%%

"part" { return TOK_STR; }
"fn"   { return TOK_INT; }

%%

