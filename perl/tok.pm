#!/usr/bin/perl

package tok;

no warnings "once";

$TK_SPACE = 256+1;
$TK_DIGIT = 256+2;
$TK_IDENT = 256+3;
$TK_STR   = 256+4;

$TK_IF    = 256+5;
$TK_THEN  = 256+6;
$TK_ELSE  = 256+7;
$TK_PART  = 256+8;
$TK_LOGIC = 256+9;
$TK_FSM   = 256+10;

$TK_LSHIFT = 256+100;
$TK_RSHIFT = 256+101;
$TK_ARROW  = 256+102;

$TK_INOUT  = 256+103;
$TK_IN     = 256+104;
$TK_OUT    = 256+105;

$TK_THORN  = 256+106;
$TK_OMEGA  = 256+107;
$TK_CURSOR = 256+108;

#$DIGIT = 256+2;

sub trace {

}

sub new {
    my ($c,$all,$pos,$tok,$id) = @_;
    my $s = {'tok'=>$tok,'id'=>$id};
    bless $s,$c;
}

use overload
    '""' => \&stringify;

sub escapechr {
    my ($e) = @_;
    return "\\'" if ($e eq "'");
    return "\\\\" if ($e eq "\\") ;
    return $e;
}

sub escapestr {
    my ($e) = @_;
    $e =~ s/"/\\"/g;
    return $e;
}

sub stringify {
    my ($v) = @_;
    my ($n) = ("undef<$$v{'id'}>");
    my @toks = grep { /^TK_/ } keys(%tok::);
    if ($$v{'id'} <=255) {
	$n = "ord('".escapechr(chr($$v{'id'}))."')";
    } else {
	foreach my $e (@toks) {
	    #print ("test $e: ".${$tok::{$e}}." \n");
	    if (${$tok::{$e}} == $$v{'id'}) {
		$n = $e;
		last;
	    }
	}
    }

    return sprintf('TOK(%s, "%s")',$n, escapestr($$v{'tok'}));
}

1;

package lex;

my %keywords = (
    "if"   => $tok::TK_IF,
    "then" => $tok::TK_THEN,
    "else" => $tok::TK_ELSE,
    "part" => $tok::TK_PART,
    "logic" => $tok::TK_LOGIC,
    "fsm"  => $tok::TK_FSM,
    "<<"   => $tok::TK_LSHIFT,
    ">>"   => $tok::TK_RSHIFT,
    "->"   => $tok::TK_ARROW,
    "«»"   => $tok::TK_INOUT,
    "»"    => $tok::TK_IN,
    "«"    => $tok::TK_OUT,
    "þ"    => $tok::TK_THORN,
    "Ω"    => $tok::TK_OMEGA,
    "¶"    => $tok::TK_CURSOR
    );

my $kwreg = join("|",keys(%keywords));

sub lex
{
    my ($a) = @_;
    pos($a) = 0;
    my @a = ();
    while(length($a)>pos($a)) {
    	my ($lastpos,$tokid) = (pos($a), undef);
	#print("From ".pos($a)."\n");
	if ($a =~ m/\G[\s\n]+/gcms) {
	    next;
	} elsif ( $a =~ m/\G[0-9]+/gcms) {
	    $tokid = $tok::TK_DIGIT;
	} elsif ( $a =~ m/\G(?:
		     [a-zA-Z_][a-zA-Z_]* |
		     ->
		  )
		  /gcmsx) {
	    $tokid = $tok::TK_IDENT;
	    if (defined($keywords{$&})) {
		$tokid = $keywords{$&};
	    }
	} elsif ( $a =~ m/\G(?:
		  «»|
		  ¶|
		  «|
		  »|
		  þ|
		  Ω|
		  <<|
		  >>|
		  ->)/gcmsx) {
	    $tokid = $keywords{$&};
	} elsif ( $a =~ m/\G(?:
		  [
		  \[
		  \]
		  \{
		  \}
		  \(
		  \)
		  :
		  ;
		  \+
		  \-
		  |
		  &
		  <
		  >
		  ,
		  \.
		  ])
		  /gcmsx) {
	    $tokid = ord($&);
	} elsif ( $a =~ m/\G"(?:[^\\"]+|\\.)*"/gcmsx)
	{
	    $tokid = $tok::TK_STR;
	}
	else {
	    croak("Can't parse	'".substr($a,pos($a))."'");
	}
	my $i = ::tok->new($a,$lastpos,$&,$tokid);
	push(@a, $i);
    }
    #print (Dumper(\@a));
    return @a;
}

1;
