#!/usr/bin/perl
if (!$::Driver) { use FindBin; exec("./driver.pl", @ARGV, $0); die; }
# $Id$
# DESCRIPTION: Verilator: Verilog Test driver/expect definition
#
# Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.

compile (
	 fails=>1,
	 expect=>
'%Error: t/t_case_x_bad.v:\d+: Use of x/. constant in case statement, \(perhaps intended casex/casez\)
%Error: Exiting due to.*',
	 );

ok(1);
1;
