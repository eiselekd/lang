#!/usr/bin/rakudo
use v6;

multi sub a($b) {
    say "+a($b)";
}

multi sub a(1) {
    say "a(1)";
}

multi sub a(2) {
    say "a(2)";
}

a(1);
a(2);
a(3);
