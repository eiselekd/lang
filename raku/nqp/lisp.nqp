use NQPHLL;
# major-mode

grammar Lisp::Grammar is HLL::Grammar {
    token TOP {
        :my $*CUR_BLOCK := QAST::Block.new(QAST::Stmts.new());
        :my $*TOP_BLOCK   := $*CUR_BLOCK;
        ^ ~ $ <sexplist>
            || <.panic('Syntax error')>
    }

    token variable { '$' <ident> }
    token num { \d+ [ '.' \d+ ]? }
    # quote_EXPR ってのはなんかプリセットっぽいやつ｡
    token str { <?["]> <quote_EXPR: ':qq'> }
    token op { '~' | '+' | '-' | '*' | '/' | 'print' | 'say' | 'exit' | 'abort' | 'time' | 'sha1' }

    proto token func {*}
    rule func:sym<define> { '(' 'define' <variable> <exp> ')' }
    # (if cond then else)
    rule func:sym<if> { '(' 'if' <exp> <exp> <exp>? ')' }
    rule func:sym<op> { '(' <op> <exp>* ')' }

    # こういうふうに書くと､multi dispatch っぽくできる｡
    proto token exp {*}
    rule exp:sym<func> { <func> }
    rule exp:sym<num>  { <num>  }
    rule exp:sym<str>  { <str>  }
    rule exp:sym<variable>  { <variable>  }

    rule sexplist { <exp>* }
}

class Lisp::Actions is HLL::Actions {
    method TOP($/) {
        $*CUR_BLOCK.push($<sexplist>.ast);
        make QAST::CompUnit.new( $*CUR_BLOCK );
    }

    method sexplist($/) {
        my $stmts := QAST::Stmts.new( :node($/) );

        for $<exp> {
            $stmts.push($_.ast)
        }

        make $stmts;
    }

    method exp:sym<num>($/) {
        make QAST::NVal.new(:value(nqp::numify($/.Str)));
    }

    method exp:sym<str>($/) {
        make $<str>.ast;
    }

    method exp:sym<func>($/) {
        make $<func>.ast;
    }

    method exp:sym<variable>($/) {
        make $<variable>.ast;
    }

    method variable($/) {
        make QAST::Var.new(:name(~$/), :scope('lexical'));
    }

    method str($/) {
        make $<quote_EXPR>.ast;
    }

    method func:sym<define>($/) {
        make QAST::Op.new(
            :op<bind>,
            QAST::Var.new(:name($<variable>), :scope('lexical'), :decl('var')),
            $<exp>.ast
        );
    }

    method func:sym<if>($/) {
        my $op := QAST::Op.new(
            :op<if>,
            block_immediate(QAST::Block.new($<exp>[0].ast)),
            block_immediate(QAST::Block.new($<exp>[1].ast)),
            :node($/)
        );
        if nqp::elems($<exp>) == 3 {
            $op.push(block_immediate(QAST::Block.new($<exp>[2].ast)));
        }
        make $op;
    }

    # Ref. src/NQP/Actions.nqp
    sub block_immediate($block) {
        $block.blocktype('immediate');
        unless $block.symtable() {
            my $stmts := QAST::Stmts.new( :node($block.node) );
            for $block.list { $stmts.push($_); }
            $block := $stmts;
        }
        $block;
    }

    method func:sym<op>($/) {
        if $<op> eq "+" {
            my $ast := QAST::IVal.new(:value(0));
            for $<exp> {
                $ast := QAST::Op.new(:op<add_n>, $ast, $_.ast);
            }
            make $ast;
        } elsif $<op> eq "~" {
            # これ､動きそうだけど全く動かない｡
            my $ast := nqp::shift($<exp>);
            for $<exp> {
                $ast := QAST::Op.new(:op<concat>, $ast, $_.ast);
            }
            make $ast;
        } elsif $<op> eq "-" {
            my $ast := (
                nqp::elems($<exp>) <= 1
                ?? QAST::IVal.new(:value(0))
                !! nqp::shift($<exp>).ast
            );
            for $<exp> {
                $ast := QAST::Op.new(:op<sub_n>, $ast, $_.ast);
            }
            make $ast;
        } elsif $<op> eq "*" {
            my $ast := QAST::IVal.new(:value(1));
            for $<exp> {
                $ast := QAST::Op.new(:op<mul_n>, $ast, $_.ast);
            }
            make $ast;
        } elsif $<op> eq "/" {
            if $<exp> >= 2 {
                my $ast := nqp::shift($<exp>).ast;
                for $<exp> {
                    $ast := QAST::Op.new(:op<div_n>, $ast, $_.ast);
                }
                make $ast;
            } else {
                nqp::die("Wrong number of arguments for '/'");
            }
        } elsif $<op> eq "say" {
            my $stmts := QAST::Stmts.new( :node($/) );
            for $<exp> {
                $stmts.push(
                    QAST::Op.new(
                        :op<say>,
                        $_.ast
                    )
                );
            }
            make $stmts;
        } elsif $<op> eq "sha1" {
            if +$<exp> == 1 {
                make QAST::Op.new(:op<sha1>, $<exp>[0].ast);
            } else {
                nqp::die("Wrong arguments count for 'sha1'");
            }
        } elsif $<op> eq "time" {
            if +$<exp> == 0 {
                make QAST::Op.new(:op<time_n>);
            } else {
                nqp::die("Too much arguments for 'time'");
            }
        } elsif $<op> eq "exit" {
            if +$<exp> == 0 {
                make QAST::Op.new(
                    :op<exit>,
                    QAST::IVal.new(:value(0))
                );
            } elsif +$<exp> == 1 {
                make QAST::Op.new(
                    :op<exit>,
                    $<exp>[0].ast
                );
            } else {
                nqp::die("Too much arguments for 'exit'");
            }
        } elsif $<op> eq "print" {
            my $stmts := QAST::Stmts.new( :node($/) );
            for $<exp> {
                $stmts.push(
                    QAST::Op.new(
                        :op<print>,
                        $_.ast
                    )
                );
            }
            make $stmts;
        } else {
            nqp::die("Unknown function name: " ~ $<op>);
        }
    }
}

class Lisp::Compiler is HLL::Compiler {
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
    my $comp := Lisp::Compiler.new();
    $comp.language('lisp');
    $comp.parsegrammar(Lisp::Grammar);
    $comp.parseactions(Lisp::Actions);
    $comp.command_line(@ARGS, :encoding('utf8'));
}
