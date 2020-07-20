#!/usr/bin/rakudo
# Local Variables:
# checker-enable:1
# End:
use Test;

grammar p {

    token O(*%spec) {
        :my %*SPEC := %spec;
        <?>
    }

    proto token term { <...> }
    proto token infix { <...> }
    proto token prefix { <...> }
    proto token postfix { <...> }
    proto token postcircumfix { <...> }

    rule EXPR { <termish> [<infixish> <termish>]* }

    token termish { <prefixish>* <term> <postfixish>* }

    token infixish   { <OPER=infix> }
    token prefixish  { <OPER=prefix> <.ws> }
    token postfixish {
        | <OPER=postfix>
	| <OPER=postcircumfix>
    }

    token infix:sym<*> { <sym> <O(:prec<u=>, :assoc<left>, :op<mul_n>)> }
    token infix:sym<+> { <sym> <O(:prec<t=>, :assoc<left>, :op<add_n>)> }

    token term:sym<num>           { <num> }
    token num { \d+ }


};

my $a = p.parse( "1+2*3", :rule('EXPR') );
say $a;
#my $b = p.parse( "1*2+3", :rule('EXPR') );
#say $b;
