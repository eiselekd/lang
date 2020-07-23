#!/usr/bin/raudo
use Test;

role a0 [ Str $member ] {

    multi method AT-KEY ($key)
    {
	return self!<$member>[$key];
    }
    multi method EXISTS-KEY ($key)
    {
	say "EXISTS-KEY";
    }
}

class a1 does a0["@ar"] {
    has @.ar ;
}

my $a = a1.new();

$a<'a'>;
