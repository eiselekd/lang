#!/usr/bin/rakudo
# Local Variables:
# checker-enable:1
# End:
use Test;

# see: https://github.com/Apress/perl-6-regexes-and-grammars/blob/master/chapter-13-case-studies/operator-precedence-parser-class.p6

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
    token prefixish  { <prefix> <.ws> }
    token postfixish {
        | <postfix>
	| <postcircumfix>
    }

    token infixish   { <infix> }
    token infix:sym<*> { <sym> <O(:prec<u=>, :assoc<left>, :op<mul_n>)> }
    token infix:sym<+> { <sym> <O(:prec<t=>, :assoc<left>, :op<add_n>)> }

    token term:sym<num>           { <num> }
    token num { \d+ }

};

class pa {

    sub opp-reduce(@stack) {
        my ($term1, $op, $term2) = @stack.splice(*-3, 3);
        @stack.push([~$op<infix><sym>, $term1, $term2]);
    }

    method O($/) {
	make %*SPEC;
    }
    method termish ($/) { make $<term>.made; }
    method EXPR ($/) {
	my @ops = $/<infixish>;
	my @terms  = $/<termish>;

	my @stack = (shift @terms).made;
	for @ops Z @terms -> [$op, $term] {
            opp-reduce(@stack) while (@stack >= 3
				      &&
				      $op<infix><O>.made<prec> lt
				      @stack[*-2]<infix><O>.made<prec>);
            @stack.push($op, $term.made);
        }

	opp-reduce(@stack) while @stack > 1;
        make @stack[0];
    }
    method term:sym<num>($/) { make $<num>.made }
    method num($/) { make $/.Int ; }
};

#

is-deeply p.parse( "3+2*1", :rule('EXPR'), :actions(pa) ).made, ["+", 3, ["*", 2, 1]], "3+2*1";
is-deeply p.parse( "3*2+1", :rule('EXPR'), :actions(pa) ).made, ["+", ["*", 3, 2], 1], "3*2+1";

#my $b = p.parse( "1*2+3", :rule('EXPR') );
#say $b;
