#!/usr/bin/perl
if (!$::Driver) { use FindBin; exec("./driver.pl", @ARGV, $0); die; }
# $Id$
# DESCRIPTION: Verilator: Verilog Test driver/expect definition
#
# Copyright 2003-2005 by Wilson Snyder. This program is free software; you can
# redistribute it and/or modify it under the terms of either the GNU
# General Public License or the Perl Artistic License.

compile (
	 v_flags2 => [$Last_Self->{v3}?'--assert':($Last_Self->{nc}?'+assert':'')],
	 );

execute (
	 check_finished=>1,
	 );

ok(1);
1;
