grammar N {
    token TOP {
        <digit>+
    }
}

for <1 42 123 1000> -> $n {
    say N.parse($n) ?? "OK $n" !! "NOT OK $n";
}
