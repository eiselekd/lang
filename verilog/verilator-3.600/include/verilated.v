// $Id$ -*- C++ -*-
//*************************************************************************
//
// Code available from: http://www.veripool.com/verilator
//
//*************************************************************************
//
// Copyright 2003-2006 by Wilson Snyder. This program is free software; you can
// redistribute it and/or modify it under the terms of either the GNU
// General Public License or the Perl Artistic License.
//
// This is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
//=========================================================================
//
// DESCRIPTION: Verilator: Include in verilog files to hide verilator defines

`ifdef _VERILATED_V_ `else
 `define _VERILATED_V_ 1

 // Hide verilator pragmas from other tools
 `ifdef verilator `else
  `define coverage_block_off
 `endif
 
 // Hide file descriptor difference
 `ifdef verilator
  `define verilator_file_descriptor reg [63:0]
 `else
  `define verilator_file_descriptor integer
 `endif

`endif // guard
