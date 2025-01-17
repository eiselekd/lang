// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   clk
   );
   parameter PAR = 3;
   
   m1 #(PAR) m1();
   m3 #(PAR) m3();

   input clk;
   integer cyc=1;
   reg [4:0] bitsel;

   always @ (posedge clk) begin
      cyc <= cyc + 1;
      if (cyc==0) begin
	 bitsel = 0;
	 if (PAR[bitsel]!==1'b1) $stop;
	 bitsel = 1;
	 if (PAR[bitsel]!==1'b1) $stop;
	 bitsel = 2;
	 if (PAR[bitsel]!==1'b0) $stop;
      end
      if (cyc==1) begin
	 $write("*-* All Finished *-*\n");
	 $finish;
      end
   end

endmodule

module m1;
   parameter PAR1 = 0;
   m2 #(PAR1-1) m2 ();
endmodule

module m2;
   parameter PAR2 = 10;
   initial begin
      $display("%x",PAR2);
      if (PAR2 !== 2) $stop;
   end
endmodule

module m3;
   localparam LOC = 13;
   parameter PAR = 10;
   initial begin
      $display("%x %x",LOC,PAR);
      if (LOC !== 13) $stop;
      if (PAR !== 3) $stop;
   end
endmodule
