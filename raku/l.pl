#!/usr/bin/raku
use v6;
# https://andrewshitov.com/2020/03/22/chapter-5-working-on-grammar/
use Grammar::Tracer;

my %var;

grammar Lingua  {
    rule TOP {
	<statement>* %% ';'
    }

    rule statement {
	| <variable-declaration>
	| <assignment>
	| <function-call>
    }

    rule variable-declaration {
	'my' <variable-name> {
            %var{$<variable-name>} = 0;
	}
    }

    rule assignment {
	<variable-name> '=' <value> {
	    %var{~$<variable-name>} = +$<value>;
	}
    }

    rule function-call {
	<function-name> <variable-name> {
	    say $<function-name>;
            say "val: " ~ %var{~$<variable-name>}
            if $<function-name> eq 'say';
	}
    }

    token variable-name {
	\w+
    }

    token value {
	\d+
    }

    token function-name {
	'say'
    }


}

say Lingua.parse("my a;
a=1;say a;
");

say %var;
