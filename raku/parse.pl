#!/usr/bin/rakudo
# Local Variables:
# checker-enable:1
# End:
use v6;
use Test;

grammar Calculator {

    token TOP { [ <add> | <sub> ] }
    rule  add { <num> '+' <num> }
    rule  sub { <num> '-' <num> }
    token num { \d+ }
}

class Calculations {
    method TOP ($/) { make $<add> ?? $<add>.made !! $<sub>.made; }
    method add ($/) { make [+] $<num>; }
    method sub ($/) { make [-] $<num>; }
}

ok(Calculator.parse('2 + 3'));
say Calculator.parse('2 + 3', actions => Calculations).made;
