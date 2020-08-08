#!/usr/bin/raudo
use Test;
# https://stackoverflow.com/questions/63055923/how-to-access-attributes-in-object-dynamically-in-raku

class c0 {
    has $!a0 = 1;
    has $!a1 = 2;
    method access(Str $m) {
	return self!"$m"();
	if ($m eq "a0") { return $!a0; }
	if ($m eq "a1") { return $!a1; }
    }
    method set(Str $m, $v) {

	if ($m eq "a0") { $!a0 = $v; }
	if ($m eq "a1") { $!a1 = $v; }
    }
}

my $c = c0.new();
$c.set("a0",3);
$c.set("a1",4);
say $c.access("a0");
say $c.access("a1");
