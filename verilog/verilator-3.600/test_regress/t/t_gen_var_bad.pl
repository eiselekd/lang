#!/usr/bin/perl
if (!$::Driver) { use FindBin; exec("./driver.pl", @ARGV, $0); die; }
# $Id$
# DESCRIPTION: Verilator: Verilog Test driver/expect definition
#
# Copyright 2003 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.

compile (
	 fails=>1,
	 expect=>
'.*%Error: t/t_gen_var_bad.v:\d+: Non-genvar used in generate for: i
%Error: Exiting due to.*',
	 );

ok(1);
1;
