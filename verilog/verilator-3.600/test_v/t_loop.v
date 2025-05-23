// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

module t_loop (/*AUTOARG*/
   // Outputs
   passed, 
   // Inputs
   clk
   );

   input clk;
   output passed;  reg passed; initial passed = 0;
   reg [7:0] cyc; initial cyc=0;

   reg [31:0] loops;
   reg [31:0] loops2;
   integer   i;

   always @ (posedge clk) begin
      cyc <= cyc+8'd1;
      if (cyc == 8'd1) begin
	 $write("[%0t] t_loop: Running\n",$time);
	 // Unwind <
	 loops = 0;
	 loops2 = 0;
	 for (i=0; i<16; i=i+1) begin
	    loops = loops + i;		// surefire lint_off_line ASWEMB
	    loops2 = loops2 + i;	// surefire lint_off_line ASWEMB
	 end
	 if (i !== 16) $stop;
	 if (loops !== 120) $stop;
	 if (loops2 !== 120) $stop;
	 // Unwind <=
	 loops = 0;
	 for (i=0; i<=16; i=i+1) begin
	    loops = loops + 1;
	 end
	 if (i !== 17) $stop;
	 if (loops !== 17) $stop;
	 // Don't unwind breaked loops
	 loops = 0;
	 for (i=0; i<16; i=i+1) begin
	    loops = loops + 1;
	    if (i==7) i=99;	// break out of loop
	 end
	 if (loops !== 8) $stop;
	 // Don't unwind large loops!
	 loops = 0;
	 for (i=0; i<100000; i=i+1) begin
	    loops = loops + 1;
	 end
	 if (loops !== 100000) $stop;
	 //
	 $write("[%0t] t_loop: Passed\n",$time);
	 passed <= 1'b1;
      end
   end

endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
