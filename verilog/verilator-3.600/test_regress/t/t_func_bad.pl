#!/usr/bin/perl
if (!$::Driver) { use FindBin; exec("./driver.pl", @ARGV, $0); die; }
# $Id:$
# DESCRIPTION: Verilator: Verilog Test driver/expect definition
#
# Copyright 2003 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.

compile (
	 fails=>1,
	 expect=>
'%Error: t/t_func_bad.v:\d+: Too few arguments in function call
%Error: t/t_func_bad.v:\d+: Too many arguments in function call
%Error: t/t_func_bad.v:\d+: Too few arguments in function call
%Error: t/t_func_bad.v:\d+: Unsupported: Task output pin connected to non-variable
%Error: t/t_func_bad.v:\d+: Outputs not allowed in function declarations
%Error: Exiting due to',
	 );

ok(1);
1;

