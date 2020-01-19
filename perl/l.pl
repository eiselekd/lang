#!/usr/bin/perl
# Local Variables:
# checker-enable:1
# End:

use Test::More;
use File::Slurp;
use Data::Dumper;
use Carp qw( croak );
use FindBin qw($Bin);

sub test{
    $a = 10;
    ok(1==2);
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
