use NQPHLL;

grammar Arith::Grammar is HLL::Grammar {
    token TOP {
	<EXPR>
    }

    # mult: :prec<u=>, :assoc<left>
    # add : :prec<t=>, :assoc<left>
    #token infix:sym<*> { <sym> <O(|%multiplicative, :op<mul_n>)> }
    #token infix:sym<+> { <sym> <O(|%additive,       :op<add_n>)> }
    token infix:sym<*> { <sym> <O(:prec<u=>, :assoc<left>, :op<mul_n>)> }
    token infix:sym<+> { <sym> <O(:prec<t=>, :assoc<left>, :op<add_n>)> }

    token term:sym<value> { <value>  }

    proto token value {*}
    token value:sym<integer> { '-'? \d+ }
}


class Arith::Actions is HLL::Actions {
    method TOP($/) {

    }

    method term:sym<value>($/) { make $<value>.ast; }
}

class Arith::Compiler is HLL::Compiler {
    method eval($code, *@_args, *%adverbs) {
	say(" * code list '$code'");
        my $output := self.compile($code, :compunit_ok(1), |%adverbs);

        if %adverbs<target> eq '' {
            my $outer_ctx := %adverbs<outer_ctx>;
            $output := self.backend.compunit_mainline($output);
            if nqp::defined($outer_ctx) {
                nqp::forceouterctx($output, $outer_ctx);
            }

            $output := $output();
        }

        $output;
    }
}

sub MAIN(*@ARGS) {
    my $comp := Arith::Compiler.new();
    $comp.language('lisp');
    $comp.parsegrammar(Arith::Grammar);
    $comp.parseactions(Arith::Actions);
    $comp.command_line(@ARGS, :encoding('utf8'));
}
