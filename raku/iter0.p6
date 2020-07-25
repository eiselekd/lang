#!/usr/bin/raudo

sub f( \x ) {
    return \x[1];
}

my @a = [ 1,2,3 ];

my $b = f(\ @a);
say $b;
say @a;
$b = 4;
say @a;


#is-deeply( $a0, [ 1, 2 ], "test0");
