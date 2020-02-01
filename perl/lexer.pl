#!/usr/bin/perl
# Local Variables:
# checker-enable:1
# End:

use Test::More;
use File::Slurp;
use Data::Dumper;
use Carp qw( croak );
use FindBin qw($Bin);
require "${Bin}/tok.pm";
no warnings "once";

my $file_content = read_file('lang.txt');
my @a = lex::lex($file_content);
print(join("\n",@a));

sub ok_lex {
    my ($m, $b) = @_;
    my @a = lex::lex($m);
    #print(join("\n",@a));

    for (my $i = 0; $i < scalar(@a); $i++) {
	#print($i.":".$a[$i]{'id'}.":".ref($$b[$i])."\n");
	if (!($a[$i]{'id'} == $$b[$i])) {
	    #print ($a[$i]{'id'}."==".$$b[$i]."\n");
	    return 0;
	}
	#ok(1==2);
    }
    return 1;
}

ok(ok_lex("1+1",[$tok::TK_DIGIT, ord('+'), $tok::TK_DIGIT]));
ok(ok_lex("{}()",[ord('{'), ord('}'), ord('('), ord(')')]));
ok(ok_lex("if then else",[$tok::TK_IF, $tok::TK_THEN, $tok::TK_ELSE]));
ok(ok_lex("a.b,c",[$tok::TK_IDENT, ord('.'), $tok::TK_IDENT, ord(','), $tok::TK_IDENT]));
ok(ok_lex("< << >> >",[ord('<'), $tok::TK_LSHIFT, $tok::TK_RSHIFT, ord('>')]));
ok(ok_lex("->",[$tok::TK_ARROW]));
ok(ok_lex("« «» »",[$tok::TK_OUT, $tok::TK_INOUT, $tok::TK_IN]));

sub test{
    $a = 10;
    ok(1==1);
}

test();

done_testing;

#print "keys:".join("\n",keys(%tok::))

# (flycheck-mode)
# (require 'utils/perl-checker.el)
# (flycheck-add-next-checker (flycheck-get-checker-for-buffer) '(warning . utils/perl-checker-makefile-checker ))
# (gdb-many-windows)

# (require 'gud)
# (require 'gdb-ui)
# (require 'perldb-ui)
# (require 'perldb-ui-ex)
# (perldb-ui)

# (require 'ediff)
# (require 'realgud)
#
# (realgud:perldb)
# (realgud:trepan.pl)
# (realgud:trepan2)
# (perldb-put-breakpoint "lexer.pl" 15)
# (perldb-find-breakpoints "lexer.pl" )
