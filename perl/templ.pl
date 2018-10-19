#!/usr/bin/perl
use Getopt::Long;
use File::Basename;
use File::Path;
use FindBin qw($Bin);
use Cwd;
use Data::Dumper;
use Carp;
use Cwd 'abs_path';
use lib "$Bin/../lib";
require "$Bin/templ.pm";
require "$Bin/templ_objs.pm";

$obj1 = new templ::obj1([map { new templ::obj2() } (0..9)]);

my $m = $obj1->doSub($$obj1{'txt'});
print($m);

