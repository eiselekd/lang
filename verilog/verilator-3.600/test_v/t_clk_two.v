// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

`include "verilated.v"

module t_clk_two (/*AUTOARG*/
   // Inputs
   fastclk, reset_l
   );
   input fastclk;
   input reset_l;
   // verilator lint_off GENCLK
   reg clk2;
   // verilator lint_on GENCLK
   reg [31:0] count;

   wire reset_h = ~reset_l;
   always @ (posedge fastclk) begin
      if (reset_h) clk2 <= 0;
      else clk2 <= ~clk2;
   end
   always @ (posedge clk2) begin
      if (reset_h) count <= 0;
      else count <= count + 1;
   end
endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
