#!/usr/bin/perl
# Local Variables:
# checker-enable:1
# End:

use Test::More;
use File::Slurp;
use Carp qw( croak );

my $file_content = read_file('lang.txt');

sub lex
{
    my ($a) = @_;
    pos($a) = 0;
    while(length($a)>pos($a)) {
	print("From ".pos($a)."\n");
	if ($a =~ m/\G[\s\n]+/gcms) {
	    #
	} elsif ( $a =~ m/\G[0-9]+/gcms) {
	    print ("Found digit '$&'\n");
	} elsif ( $a =~ m/\G(?:
		  if|
		  then|
		  else|
		  part)
		  /gcmsx) {
	    print ("Found keyword '$&'\n");
	} elsif ( $a =~ m/\G[a-zA-Z_][a-zA-Z_]*/gcms) {
	    print ("Found ident '$&'\n");
	} elsif ( $a =~ m/\G(?:
		  [\{\}\(\):])
		  /gcmsx) {
	    print ("Found single-char '$&'\n");
	} else {
	    croak("Can't parse	'".substr($a,pos($a))."'");
	}

    }

}

lex($file_content);

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
