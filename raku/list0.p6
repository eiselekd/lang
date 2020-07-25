#!/usr/bin/raudo
use Test;

my @a = [ 1, 2, 3 ];
my @b = @a».Int;
say @b;
