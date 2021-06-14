#!/usr/bin/raudo
use Test;

my @a = [ 1, 2, 3 ];
my @b = @a».Int;
say @b;

#my %f = %( 'a' => 1, 'b' => 2 );
#my %b = %( 'a' => 2, 'c' => 3 );

#say (%f >>+>> %b);

%(a=>1,b=>2,c=>3).map: -> $x {
    say $x
};
