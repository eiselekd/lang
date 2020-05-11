#!/usr/bin/rakudo
use v6;

class a {
    method n { say ("a.n"); };
};

my $_a = a.new();
my $n = 'n';
$_a.'n'();
$_a."$n"();

my %a = (:a<b>:c("d"):e(1));
say (%a);
say (%a{"c"}:exists);
say (%a<c>:exists);
say (%a<c>:delete);
say (%a<c>:exists);
