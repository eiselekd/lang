package Tie::AppendHash;
use Tie::Hash;
our @ISA = ("Tie::StdHash");
sub STORE {
    my ($self, $key, $value) = @_;
    push @{$self->{key}}, $value;
}
1;


package Tie::Folded;
use strict;
use Tie::Hash;
use vars qw(@ISA);
@ISA = qw(Tie::StdHash);
sub STORE {
    my ($self, $key, $value) = @_;
    return $self->{lc $key} = $value;
    } 
sub FETCH {
    my ($self, $key) = @_;
    return $self->{lc $key};
} 
sub EXISTS {
    my ($self, $key) = @_;
    return exists $self->{lc $key};
} 
sub DEFINED {
    my ($self, $key) = @_;
    return defined $self->{lc $key};
} 
1;


tie %h, 'Foo', 'a' => 1
$obj = Foo-->TIEHASH('a',1);
$h{a}
$obj-->FETCH ('a')
$h{a} = 1
$obj-->STORE ('a', 1)
delete $h{a}
$obj-->DELETE('a')
exists $h{a}
$obj-->EXISTS('a')
keys (%h),values(%h)
each (%h)
$lk = $obj-->FIRSTKEY ();
do {
   $val = $obj-->FETCH{$lk};
} while ($lk = $obj-->NEXTKEY($lk));
%h = ()
$obj-->CLEAR()
%h = (a=> 1)
$obj-->CLEAR()
$obj-->STORE('a',1)
untie %h
$obj-->DESTROY()
