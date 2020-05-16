use v6;

my $has-color = True;
try {
    require Terminal::ANSIColor;
    CATCH { default { $has-color = False; } };
};

say $has-color;
