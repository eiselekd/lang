#!/usr/bin/perl

package tok;
use constant {
    SPACE => 256+1,
    DIGIT => 256+2,
    IDENT => 256+3,

    IF    => 256+4,
    THEN  => 256+5,
    ELSE  => 256+6,
    PART  => 256+7
};

#$DIGIT = 256+2;

sub trace {

}

sub new {
    my ($c,$all,$pos,$tok,$id) = @_;
    my $s = {'tok'=>$tok,'id'=>$id};
    bless $s,$c;
}

1;
