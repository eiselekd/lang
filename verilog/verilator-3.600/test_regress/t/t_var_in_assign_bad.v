// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2005 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   value
   );
   input [3:0] value;
   assign      value = 4'h0;
   sub sub (.valueSub	(value[3:0]));
endmodule

module sub (/*AUTOARG*/
   // Inputs
   valueSub
   );
   input [3:0] valueSub;
   assign      valueSub = 4'h0;
endmodule
