// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

module t_inst_b (/*AUTOARG*/
   // Outputs
   o_seq_d1r, o_com, o2_com, 
   // Inputs
   clk, i_seq, i_com, i2_com, wide_for_trace, wide_for_trace_2
   );
   // verilator inline_module

   input clk;

   input  	i_seq;
   output	o_seq_d1r;
   input  	i_com;
   output	o_com;
   input [1:0] 	i2_com;
   output [1:0]	o2_com;
   input [127:0] wide_for_trace;
   input [127:0] wide_for_trace_2;

   /*AUTOREG*/
   // Beginning of automatic regs (for this module's undeclared outputs)
   // End of automatics

   reg 			o_seq_d1r;
   always @ (posedge clk) begin
      o_seq_d1r <= ~i_seq;
   end

   wire [1:0] o2_com = ~i2_com;
   wire       o_com = ~i_com;

endmodule

// Local Variables:
// compile-seqmand: "./vlint t_inst_b.v"
// End:
