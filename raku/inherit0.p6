#!/usr/bin/raudo
use Test;
# https://docs.raku.org/language/classtut
class port {
    has $.dir;
    has $.portname;
    has $.porttype;
    method toStruct {
	return { :dir(self.dir), :portname(self.porttype) };
    }
};

class ports {
    has @.ports_ ;
};

my $p = ports.new(:ports_<[2]>);



#is-deeply( $a0, [ 1, 2 ], "test0");
