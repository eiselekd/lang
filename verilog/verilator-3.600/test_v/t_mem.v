// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

module t_mem (/*AUTOARG*/
   // Outputs
   passed, 
   // Inputs
   clk
   );

   input clk;
   output passed;  reg passed; initial passed = 0;
   integer _mode;	initial _mode = 0;

   reg [7:0]  mem_narrow [0:31];  //surefire lint_off_line RD_WRT WRTWRT NBAJAM
   reg [77:0] mem_wide   [1024:0];  //surefire lint_off_line RD_WRT WRTWRT NBAJAM
   reg [7:0]  mem_dly_narrow [0:1];  //surefire lint_off_line RD_WRT WRTWRT NBAJAM
   reg [77:0] mem_dly_wide   [1:0];  //surefire lint_off_line RD_WRT WRTWRT NBAJAM
   reg [34:0] vec_wide;

   reg [31:0] wrd0 [15:0];
   wire [3:0] sel = 4'h3;
   wire [31:0] selout = wrd0[sel];
 
   // Must take LSBs into account in bit extract widths.
   wire [15:14] sixt = 2'b10; // surefire lint_off_line ASWCBB
   wire [16:14] sixt2 = 3'b110; // surefire lint_off_line ASWCBB
   wire [3:0] 	sixfrom = 13;
   wire [4:0] 	sixfrom2 = 16;
   wire 	sixtext = sixt[sixfrom];
   wire 	sixtext2 = sixt2[sixfrom2];

   // Non-power of 2 memory overwriting checks
   reg [2:0] 	np2_mem   [5:0] /*verilator public*/;
   reg [2:0] 	np2_guard [7:6] /*verilator public*/;

   integer 	 i;
   
   always @ (posedge clk) begin
      if (_mode!=0) begin
	 $write("[%0t] t_mem: Running\n", $time);
	 wrd0[0] = 32'h1;
	 //
	 for (i=0; i<32; i=i+1) begin   //surefire lint_off_line STMFOR
	    mem_narrow[i] = i[7:0];
	    mem_wide[i]   = {i[7:0],70'hfeed};
	 end
	 //
	 for (i=0; i<32; i=i+1) begin   //surefire lint_off_line STMFOR
	    if (mem_narrow[i] !== i[7:0]) $stop;
	    if (mem_wide[i] !== {i[7:0],70'hfeed}) $stop;
	 end
	 //
	 vec_wide <= 0;
	 //
	 np2_guard[6] = 0;
	 np2_guard[7] = 0;
	 //
	 $write("selout %b %b %b\n", selout, sixtext, sixtext2);
      end
      if (_mode == 1) begin
	 _mode <= 2;
	 //
	 i=0;
	 mem_dly_narrow[0] <= ~i[7:0];
	 mem_dly_wide[0]   <= {~i[7:0],70'hface};
	 i=1;
	 mem_dly_narrow[i] <= ~i[7:0];
	 mem_dly_wide[i]   <= {~i[7:0],70'hface};
	 //
	 for (i=0; i<16; i=i+1) begin   //surefire lint_off_line STMFOR
	    // verilator lint_off width
	    np2_mem[i] = i[2:0]; // surefire lint_off_line ASWSBB
	    // verilator lint_on width
	    if (np2_guard[6]!=0 || np2_guard[7]!=0) $stop;
	 end
	 // lint_checking BNDMEM OFF
	 if (np2_mem[6] !== np2_mem[7]) begin
	    $write("Mem[6]!=Mem[7] during randomize...\n");
	    //$stop;  // Random value, so this can happen
	 end
	 //if (np2_mem[8] !== np2_mem[9]) $stop;  // Enhancement: Illegal indexes, make sure map to X's
	 //
	 vec_wide[32:31] <= 2'b11;
	 vec_wide[34] <= 1'b1;
	 $display("%x",vec_wide);
      end
      if (_mode == 2) begin
	 _mode <= 3;
	 //
	 for (i=0; i<2; i=i+1) begin   //surefire lint_off_line STMFOR
	    if (mem_dly_narrow[i] !== ~i[7:0]) $stop;
	    if (mem_dly_wide[i] !== {~i[7:0],70'hface}) $stop;
	 end
	 //
	 //$write ("VW %x %x\n", vec_wide[34:32], vec_wide[31:0]);
	 if (vec_wide != {4'b101_1,31'd0}) $stop;
	 //
	 $write("[%0t] t_mem: Passed\n", $time);
	 passed <= 1'b1;
      end
      _mode <= _mode + 1;
   end

endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
