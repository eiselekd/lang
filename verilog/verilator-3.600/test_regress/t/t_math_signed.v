// $Revision: 1.1 $$Date$$Author$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2004 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   clk
   );

   input clk;

   by_width #(1)   w1 (.clk(clk));
   by_width #(31) w31 (.clk(clk));
   by_width #(32) w32 (.clk(clk));
   by_width #(33) w33 (.clk(clk));
   by_width #(63) w63 (.clk(clk));
   by_width #(64) w64 (.clk(clk));
   by_width #(65) w65 (.clk(clk));
   by_width #(95) w95 (.clk(clk));
   by_width #(96) w96 (.clk(clk));
   by_width #(97) w97 (.clk(clk));

   reg 	 signed [15:0] a;
   reg 	 signed [4:0] b;

   reg 	 signed [15:0] sr,srs,sl,sls;

   reg 	[15:0] b_s;
   reg 	[15:0] b_us;

   always @* begin
      sr  = a>>b;
      srs = copy_signed(a)>>>b;
      sl  = a<<b;
      sls = a<<<b;
      // verilator lint_off WIDTH
      b_s  = b>>>4;		// Signed
      b_us = b[4:0]>>>4;	// Unsigned, due to extract
      // verilator lint_on WIDTH
   end
	       
   integer i;
   initial begin
      if ((-1 >>> 3) != -1) $stop;	// Decimals are signed
      if ((3'b111  >>> 3) != 0) $stop;	// Based numbers are unsigned
      if ((3'sb111 >>> 3) != -1) $stop;	// Signed based numbers
      if ( (3'sb000 > 3'sb000)) $stop;
      if (!(3'sb000 > 3'sb111)) $stop;
      if ( (3'sb111 > 3'sb000)) $stop;
      if ( (3'sb000 < 3'sb000)) $stop;
      if ( (3'sb000 < 3'sb111)) $stop;
      if (!(3'sb111 < 3'sb000)) $stop;
      if (!(3'sb000 >= 3'sb000)) $stop;
      if (!(3'sb000 >= 3'sb111)) $stop;
      if ( (3'sb111 >= 3'sb000)) $stop;
      if (!(3'sb000 <= 3'sb000)) $stop;
      if ( (3'sb000 <= 3'sb111)) $stop;
      if (!(3'sb111 <= 3'sb000)) $stop;
      // When we multiply overflow, the sign bit stays correct.
      if ( (4'sd2*4'sd8) != 4'd0) $stop;
      // From the spec:
      // verilator lint_off WIDTH
      i = -12 /3;     if (i !== 32'hfffffffc) $stop;
      i = -'d12 /3;   if (i !== 32'h55555551) $stop;
      i = -'sd12 /3;  if (i !== 32'hfffffffc) $stop;
      i = -4'sd12 /3; if (i !== 32'h00000001) $stop;
      // verilator lint_on WIDTH
   end

   function signed [15:0] copy_signed;
      input [15:0] ai;
      copy_signed = ai;
   endfunction

   integer cyc; initial cyc=0;
   always @ (posedge clk) begin
      cyc <= cyc + 1;
      $write("%x  %x %x %x %x  %x %x\n", cyc, sr,srs,sl,sls, b_s,b_us);
      case (cyc)
	0: begin
	   a <= 16'sh8b1b; b <= 5'sh1f;  // -1
	end
	1: begin
	   // Check spaces in constants
	   a <= 16 'sh 8b1b; b <= 5'sh01;  // -1
	end
	2: begin
	   a <= 16'sh8b1b; b <= 5'sh1e;  // shift AMOUNT is really unsigned
	end
	3: begin
	   a <= 16'sh0048; b <= 5'sh1f;
	end
	4: begin
	   a <= 16'sh4154; b <= 5'sh02;
	end
	5: begin
	   a <= 16'shc3e8; b <= 5'sh12;
	end
	6: begin
	   a <= 16'sh488b; b <= 5'sh02;
	end
	9: begin
	   $write("*-* All Finished *-*\n");
	   $finish;
	end
	default: ;
      endcase
      case (cyc)
	0: ;
	1: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh0000_ffff_0000_0000_ffff_0001) $stop;
	2: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh458d_c58d_1636_1636_0000_0000) $stop;
	3: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh0000_ffff_0000_0000_ffff_0001) $stop;
	4: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh0000_0000_0000_0000_ffff_0001) $stop;
	5: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh1055_1055_0550_0550_0000_0000) $stop;
	6: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh0000_ffff_0000_0000_ffff_0001) $stop;
	7: if ({sr,srs,sl,sls,b_s,b_us}!==96'sh1222_1222_222c_222c_0000_0000) $stop;
	8: ;
	9: ;
      endcase
   end
endmodule


module by_width (
		 input clk
		 );
   parameter 	       WIDTH=1;

   reg signed 	       i1;
   reg signed [62:0]   i63;
   reg signed [64:0]   i65;

   // verilator lint_off WIDTH
   wire signed [WIDTH-1:0] i1extp  /*verilator public*/ = i1;
   wire signed [WIDTH-1:0] i1ext  = i1;
   wire signed [WIDTH-1:0] i63ext = i63;
   wire signed [WIDTH-1:0] i65ext = i65;
   // verilator lint_on WIDTH

   integer cyc; initial cyc=0;
   always @ (posedge clk) begin
      cyc <= cyc + 1;
      i1 <= cyc[0];
      i63 <= {63{cyc[0]}};
      i65 <= {65{cyc[0]}};
      case (cyc)
	1: begin
	   if (i1extp != {WIDTH{1'b0}}) $stop;
	   if (i1ext != {WIDTH{1'b0}}) $stop;
	   if (i63ext != {WIDTH{1'b0}}) $stop;
	   if (i65ext != {WIDTH{1'b0}}) $stop;
	end
	2: begin
	   if (i1extp != {WIDTH{1'b1}}) $stop;
	   if (i1ext != {WIDTH{1'b1}}) $stop;
	   if (i63ext != {WIDTH{1'b1}}) $stop;
	   if (i65ext != {WIDTH{1'b1}}) $stop;
	end
	default: ;
      endcase
   end
endmodule
