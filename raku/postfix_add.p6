#!rakudo
class n {
    has Int $.num;
}

my @a = [ 1, 2, 3 ].map(-> $v { n.new(num=>$v); }) ;
my $a = [+] @a».num;
say $a;
