// $Id$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003-2006 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   clk
   );

   input clk;
   integer cyc; initial cyc=1;

   // verilator lint_on GENCLK
   reg [31:0] long;
   reg [63:0] quad;
   wire [31:0] longout;
   wire [63:0] quadout;
   wire [7:0] narrow = long[7:0];
   sub sub (/*AUTOINST*/
	    // Outputs
	    .longout			(longout[31:0]),
	    .quadout			(quadout[63:0]),
 	    // Inputs
	    .narrow			(narrow[7:0]),
	    .quad			(quad[63:0]));

   always @ (posedge clk) begin
      if (cyc!=0) begin
	 cyc <= cyc + 1;
	 if (cyc==1) begin
	    long <= 32'h12345678;
	    quad <= 64'h12345678_abcdef12;
	 end
	 if (cyc==2) begin
	    if (longout !== 32'h79) $stop;
	    if (quadout !== 64'h12345678_abcdef13) $stop;
	    $write("*-* All Finished *-*\n");
	    $finish;
	 end
      end
   end
endmodule

module sub (input [7:0] narrow, input [63:0] quad, output [31:0] longout, output [63:0] quadout);
   // verilator public_module
`ifdef verilator
   wire [31:0] longout = $c32("(",narrow,"+1)");
   wire [63:0] quadout = $c64("(",quad,"+1)");
`else
   wire [31:0] longout = narrow + 8'd1;
   wire [63:0] quadout = quad + 64'd1;
`endif
endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
