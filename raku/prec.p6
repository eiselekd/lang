#!/usr/bin/rakudo
# Local Variables:
# checker-enable:1
# End:
use Test;

grammar p {

    proto token infix { <...> }

    token infix:sym<*> { <sym> <O(:prec<u=>, :assoc<left>, :op<mul_n>)> }
    token infix:sym<+> { <sym> <O(:prec<t=>, :assoc<left>, :op<add_n>)> }

    token infixish { <OPER=infix> }

    method EXPR () {
	my $p;
	$p = self.infixish();
	return $p
    }

    token num { \d+ }

};

my $a = p.new();
say ($a.num());
