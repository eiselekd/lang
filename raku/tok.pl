#!/usr/bin/rakudo
use v6;
use Test;
# Local Variables:
# checker-enable:1
# End:

ok(1==1);

my @t =
'a' => 'b';

for @t -> $p {
    my $s = $p.value;
    is-deeply $s, "b",
        "Correct data structure for «{$p.key.subst(/\n/, '\n', :g)}»";
}

