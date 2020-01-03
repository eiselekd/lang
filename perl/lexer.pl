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

my $file_content = read_file('lang.txt');

my %keywords = (
    "if"   => &tok::IF,
    "then" => &tok::THEN,
    "else" => &tok::ELSE,
    "part" => &tok::PART,
    );

my $kwreg = join("|",keys(%keywords));

sub lex
{
    my ($a) = @_;
    pos($a) = 0;
    my @a = ();
    while(length($a)>pos($a)) {
    	my ($lastpos,$tokid) = (pos($a), undef);
	#print("From ".pos($a)."\n");
	if ($a =~ m/\G[\s\n]+/gcms) {
	    next;
	} elsif ( $a =~ m/\G[0-9]+/gcms) {
	    $tokid = &tok::DIGIT;
	} elsif ( $a =~ m/\G[a-zA-Z_][a-zA-Z_]*/gcms) {
	    $tokid = &tok::IDENT;
	    if (defined($keywords{$&})) {
		$tokid = $keywords{$&};
	    }
	} elsif ( $a =~ m/\G(?:
		  [\{\}\(\):])
		  /gcmsx) {
	    $tokid = ord($&);
	} else {
	    croak("Can't parse	'".substr($a,pos($a))."'");
	}
	my $i = ::tok->new($a,$lastpos,$&,$tokid);
	push(@a, $i);
    }
    #print (Dumper(\@a));
    return @a;
}

lex($file_content);

sub ok_lex {
    my ($m, $b) = @_;
    my @a = lex($m);
    for (my $i = 0; $i < scalar(@a); $i++) {
	#print($i.":".$a[$i]{'id'}.":".ref($$b[$i])."\n");
	if (!($a[$i]{'id'} == $$b[$i])) {
	    print ($a[$i]{'id'}."==".$$b[$i]."\n");
	    return 0;
	}
	#ok(1==2);
    }
    return 1;
}

ok(ok_lex("1 1",[&tok::DIGIT, &tok::DIGIT]));
ok(ok_lex("{}",[ord('{'), ord('}')]));
ok(ok_lex("if then else",[&tok::IF, &tok::THEN, &tok::ELSE]));

sub test{
    $a = 10;
    ok(1==1);
}

test();

done_testing;

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
