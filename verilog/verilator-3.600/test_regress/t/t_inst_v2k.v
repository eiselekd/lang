// $Id:$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

module t (/*AUTOARG*/
   // Inputs
   clk
   );

   input clk;
   integer cyc; initial cyc=1;

   supply0 [1:0] low;
   supply1 [1:0] high;

   reg [7:0] isizedwire;
   reg ionewire;

`ifdef never_just_for_verilog_mode
   wire oonewire;		// From sub of t_inst_v2k_sub.v
`endif

   wire [7:0]		osizedreg;		// From sub of t_inst_v2k_sub.v

   t_inst_v2k_sub sub
     (
      // Outputs
      .osizedreg			(osizedreg[7:0]),
      // verilator lint_off IMPLICIT
      .oonewire				(oonewire),
      // verilator lint_on IMPLICIT
      // Inputs
      .isizedwire			(isizedwire[7:0]),
      .ionewire				(ionewire));

   always @ (posedge clk) begin
      if (cyc!=0) begin
	 cyc <= cyc + 1;
	 if (cyc==1) begin
	    ionewire <= 1'b1;
	    isizedwire <= 8'd8;
	 end
	 if (cyc==2) begin
	    if (low != 2'b00) $stop;
	    if (high != 2'b11) $stop;
	    if (oonewire !== 1'b1) $stop;
	    if (isizedwire !== 8'd8) $stop;
	    $write("*-* All Finished *-*\n");
	    $finish;
	 end
      end
   end

endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
