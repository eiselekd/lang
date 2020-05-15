#!rakudo

sub f ($m) {
    return $m;
}

sub g ($m) {
    if f($m) -> $v {
	say "Enter $v";
    } else {
	say "No Enter";
    }
}

g(1);
g(0);
