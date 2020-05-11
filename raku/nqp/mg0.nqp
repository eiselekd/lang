use NQPHLL;

############# B ##############

grammar B::Grammar is HLL::Grammar {

    token comp_unit {
	'B'
    }
}

class B::Actions is HLL::Actions {

    method comp_unit($/) {
	my $o := QAST::Op.new(
            :op<say>,
	    QAST::SVal.new(:value('B'))
        );
	say("B");
	make $o;
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

	self.comp_unit
    }

    token comp_unit {
        'a' <bg=.LANG('B','comp_unit')>
    }
}

class A::Actions is HLL::Actions {
    method comp_unit($/) {
	say('a');
	say($<bg>.dump());
	make $<bg>.ast
    }
}

##############################

class A::Compiler is HLL::Compiler {
}

sub MAIN(*@ARGS) {
    my $comp := A::Compiler.new();
    $comp.language('a');
    $comp.parsegrammar(A::Grammar);
    $comp.parseactions(A::Actions);
    $comp.command_line(@ARGS, :encoding('utf8'));
}
