#!/usr/bin/rakudo
# Local Variables:
# checker-enable:1
# End:
use Test;

grammar pg {

    token O(*%spec) {
	:my %*SPEC := %spec;
        <?>
    }

    proto token infix { <...> }
    token infix:sym<*> { <sym> <O(:prec<u=>, :assoc<left>, :op<mul_n>)>  }


};

class pa {
    method O($/) {
	make %*SPEC;
    }
}

my $a = pg.parse( "*", :rule('infix'), :actions(pa) );
say $a<O>.^name;
say $a<O>.made.^name;
say $a<O>.made;
#my $b = p.parse( "1*2+3", :rule('EXPR') );
#say $b;
