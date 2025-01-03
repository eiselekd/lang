// $Id$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2004 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   clk
   );

   input clk;
   integer cyc; initial cyc=1;

   reg [31:0] a;
   reg [31:0] b;
   
   wire [1:0] bf;  buf   BF0 (bf[0], a[0]),
		         BF1 (bf[1], a[1]);

   // verilator lint_off IMPLICIT
   not   NT0 (nt0, a[0]);
   and   AN0 (an0, a[0], b[0]);
   nand  ND0 (nd0, a[0], b[0], b[1]);
   or    OR0 (or0, a[0], b[0]);
   nor   NR0 (nr0, a[0], b[0], b[2]);
   xor       (xo0, a[0], b[0]);
   xnor      (xn0, a[0], b[0], b[2]);
   // verilator lint_on IMPLICIT

`ifdef verilator
   specify
      specparam CDS_LIBNAME  = "foobar";
      (nt0 *> nt0) = (0, 0);
   endspecify

  specify
    // delay parameters
    specparam
      a$A1$Y = 1.0,
      b$A0$Z = 1.0;

    // path delays
    (A1 *> Q) = (a$A1$Y, a$A1$Y);
    (A0 *> Q) = (b$A0$Y, a$A0$Z);
  endspecify
`endif

   always @ (posedge clk) begin
      if (cyc!=0) begin
	 cyc <= cyc + 1;
	 if (cyc==1) begin
	    a <= 32'h18f6b034;
	    b <= 32'h834bf892;
	 end
	 if (cyc==2) begin
	    a <= 32'h529ab56f;
	    b <= 32'h7835a237;
	    if (bf[0] !== 1'b0) $stop;
	    if (bf[1] !== 1'b0) $stop;
	    if (nt0 !== 1'b1) $stop;
	    if (an0 !== 1'b0) $stop;
	    if (nd0 !== 1'b1) $stop;
	    if (or0 !== 1'b0) $stop;
	    if (nr0 !== 1'b1) $stop;
	    if (xo0 !== 1'b0) $stop;
	    if (xn0 !== 1'b1) $stop;
	 end
	 if (cyc==3) begin
	    if (bf[0] !== 1'b1) $stop;
	    if (bf[1] !== 1'b1) $stop;
	    if (nt0 !== 1'b0) $stop;
	    if (an0 !== 1'b1) $stop;
	    if (nd0 !== 1'b0) $stop;
	    if (or0 !== 1'b1) $stop;
	    if (nr0 !== 1'b0) $stop;
	    if (xo0 !== 1'b0) $stop;
	    if (xn0 !== 1'b0) $stop;
	 end
	 if (cyc==4) begin
	    $write("*-* All Finished *-*\n");
	    $finish;
	 end
      end
   end

endmodule
