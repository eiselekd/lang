#!/usr/bin/perl
# Local Variables:
# checker-enable:1
# End:

use Test::More;

sub test{
    ok(1==2);
}

test();

done_testing;

# (flycheck-mode)
# (require 'utils/perl-checker.el)
# (flycheck-add-next-checker (flycheck-get-checker-for-buffer) '(warning . utils/perl-checker-makefile-checker ))
