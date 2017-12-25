use FindBin qw($Bin);
use lib "$Bin/../lib";
require "$Bin/dotie.pm";

my $r = tie %i, "dotie";

$i{'a'} = 1;

