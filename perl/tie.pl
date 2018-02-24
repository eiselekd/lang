use FindBin qw($Bin);
use lib "$Bin/../lib";
require "$Bin/dotie.pm";

my $r = tie %i, "dotie", { a=>2 };

$i{'a'} = 1;

