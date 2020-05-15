#!rakudo

my @a = [ 1, 2, 3 ] ;
my @b = [ 4, 5, 6 ] ;
my @c = [ @a, @b ];
my @d = [ |@a, |@b ];
say @c;
say @d;
