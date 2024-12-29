// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2005 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   clk
   );

   input clk;
   integer cyc; initial cyc=1;

   wire [17:10] bitout;
   reg  [7:0] allbits;
   reg  [15:0] onebit;

   sub sub [7:0] (allbits, onebit, bitout);

   always @ (posedge clk) begin
      //$write("%x\n", bitout);
      if (cyc!=0) begin
	 cyc <= cyc + 1;
	 if (cyc==1) begin
	    allbits <= 8'hac;
	    onebit <= 16'hc01a;
	 end
	 if (cyc==2) begin
	    if (bitout !== 8'h07) $stop;
	    allbits <= 8'hca;
	    onebit <= 16'h1f01;
	 end
	 if (cyc==3) begin
	    if (bitout !== 8'h41) $stop;
	    $write("*-* All Finished *-*\n");
	    $finish;
	 end
      end
   end
endmodule

module sub (input [7:0] allbits, input [1:0] onebit, output bitout);
   wire bitout = (^ onebit) ^ (^ allbits);
endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
