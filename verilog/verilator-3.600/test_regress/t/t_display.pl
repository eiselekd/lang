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

execute (
	 check_finished=>1,
	 expect=>quotemeta(
'[0] In TOP.v: Hi
[0] In TOP.v.sub
[0] In TOP.v.sub2
[0] %X=0c %D=12 %0X=c %0O=14 %B=001100
[0] %x=0c %d=12 %0x=c %0o=14 %b=001100
[0] %x=00abbbbcccc %0x=abbbbcccc %o=00527356746314 %b=00000101010111011101110111100110011001100
[0] %x=00abc1234567812345678 %0x=abc1234567812345678 %o=012570110642547402215053170 %b=000001010101111000001001000110100010101100111100000010010001101000101011001111000

[0] %s=! %s= what! %s= hmmm!1234
'),
     );

ok(1);
1;
