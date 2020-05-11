#!/usr/bin/raku
use v6;

my $*d0 = 1;

sub a {
    say $*d0;
}

a();

sub b {
    my $*d0 = 2;
    a();
}

b;
