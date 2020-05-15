use NQPHLL;

############# B ##############

grammar B::Grammar is HLL::Grammar {

    token comp_unit {
	'B'
    }
}

class B::Actions is HLL::Actions {

    method comp_unit($/) {
        my $stmts := QAST::Stmts.new( :node($/) );
	$stmts.push(
            QAST::Op.new(
                :op<say>,
		QAST::SVal.new(:value('B'))
            )
        );
	make $stmts;
    }
}

############# A ##############

grammar A::Grammar is HLL::Grammar {

    method TOP() {

	# Language braid.
	my $*LANG := self;
	self.define_slang('MAIN', self,       self.actions);
	self.define_slang('B',    B::Grammar, B::Actions);

        # Old language braids, going away.
        my %*LANG;
        %*LANG<B>         := B::Grammar;
        %*LANG<B-actions> := B::Actions;
        %*LANG<MAIN>         := A::Grammar;
        %*LANG<MAIN-actions> := A::Actions;

        my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());

	self.comp_unit;
    }

    token comp_unit {
        'a' <bg=.LANG('B','comp_unit')> <follow>
    }

    token follow {
	'c'
    }

}

class A::Actions is HLL::Actions {

    method TOP($/) {
	say(' enter TOP');
        $*CUR_BLOCK.push($<comp_unit>.ast);
	say(' exit TOP');
        make QAST::CompUnit.new( $*CUR_BLOCK );
    }

    method comp_unit($/) {
	say(' comp_unit');
	say(":" ~ $<bg>.ast.dump());
	my $stmts := QAST::Stmts.new( :node($/) );
	$stmts.push($<bg>.ast);
	$stmts.push($<follow>.ast);
	say(" comp_unit");

	$*CUR_BLOCK.push($stmts);

	say($*CUR_BLOCK.dump());


        make QAST::CompUnit.new( $*CUR_BLOCK );


    }

    method follow($/) {
	make QAST::Op.new(
            :op<say>,
	    QAST::SVal.new(:value('c'))
        );
    }

}

##############################

class A::Compiler is HLL::Compiler {
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
    my $comp := A::Compiler.new();
    $comp.language('a');
    $comp.parsegrammar(A::Grammar);
    $comp.parseactions(A::Actions);
    $comp.command_line(@ARGS, :encoding('utf8'));
}
