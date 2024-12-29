#!/usr/bin/perl
if (!$::Driver) { use FindBin; exec("./driver.pl", @ARGV, $0); die; }
# $Id$
# DESCRIPTION: Verilator: Verilog Test driver/expect definition
#
# Copyright 2003 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.

compile (
	 );

unlink("obj_dir/t_file_test.log");

execute (
	 check_finished=>1,
     );

file_grep ("obj_dir/t_file_test.log",
qr/\[0\] hello v=12345667
\[0\] Hello2
/);

ok(1);
1;
