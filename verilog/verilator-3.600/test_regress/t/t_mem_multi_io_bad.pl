#!/usr/bin/perl
if (!$::Driver) { use FindBin; exec("./driver.pl", @ARGV, $0); die; }
# $Id$
# DESCRIPTION: Verilator: Verilog Test driver/expect definition
#
# Copyright 2003 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.

compile (
	 fails=>$Last_Self->{v3},
	 nc=>0,
	 expect=>
'%Error: t/t_mem_multi_io_bad.v:\d+: Arrayed variables may not be inputs nor outputs
%Error: Exiting due to.*',
	 );

ok(1);
1;
