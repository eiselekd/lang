%{
%}

digit  [0-9]

%%

"«»"     { return TOK_DIRINOUT; }
"»"      { return TOK_DIRIN;    }
"«"      { return TOK_DIROUT;   }

"part"   { return TOK_PART; }
"fn"     { return TOK_FN; }

{digit}+ { return TOK_INT; }

%%
