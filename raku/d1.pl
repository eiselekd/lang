
sub f {
    say (@_.perl());
};

f  [ assert  => "NoEvent",
    assert  => "NoOutput",
    send    => ("suspend", *) => True,
    send    => ("resume", 1) => True,
    command => async => UnlockThread => 0,
    assert  => "NoEvent",
    command => async => UnlockThread => 1,
    assert  => "NoEvent",
    command => async => UnlockThread => 2,
    assert  => "NoEvent",
    command => async => UnlockThread => 3,
    assert  => "NoOutput",
    command => finish => UnlockThread => 0,
    command => JoinThread => 0,
    command => Quit => 0,

]
    ;
