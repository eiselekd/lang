#!/usr/bin/raudo
use Test;

class port {
    has $.dir;
    has $.porttype;
};

class ports {
    has @.ports_ of port;
};



#is-deeply( $a0, [ 1, 2 ], "test0");
