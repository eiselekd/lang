
package dotie;

sub TIEHASH
{
    my ($class) = @_;
    
    return bless { }, $class;
}

sub STORE {
    my ($self, $key, $val) = @_;
    print("$key\n");
    if (exists $self->{hash}{$key}) {
	$self->{hash}{$key}[0] = $val;
    } else {
	push @{$self->{arr}}, $key;
	$self->{hash}{$key} = [$val, $#{$self->{arr}}];
    }
}

sub FETCH {
    my ($self, $key) = @_;
    return $self->{hash}{$key}[0];
}

1;

# tie %h, 'Foo', 'a' => 1
# $obj = Foo-->TIEHASH('a',1);

# $h{a}
# $obj-->FETCH ('a')
# $h{a} = 1
# $obj-->STORE ('a', 1)
# delete $h{a}
# $obj-->DELETE('a')
# exists $h{a}
# $obj-->EXISTS('a')
# keys (%h),values(%h)
# each (%h)
# $lk = $obj-->FIRSTKEY ();
# do {
#    $val = $obj-->FETCH{$lk};
# } while ($lk = $obj-->NEXTKEY($lk));
# %h = ()
# $obj-->CLEAR()
# %h = (a=> 1)
# $obj-->CLEAR()
# $obj-->STORE('a',1)
# untie %h
# $obj-->DESTROY()
