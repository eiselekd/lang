// $Id$
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

   reg [7:0] memory8_16 [15:0];

   reg 	      m_we;
   reg [3:1]  m_addr;
   reg [15:0] m_data;

   always @ (posedge clk) begin
      // Load instructions from cache
      memory8_16[{m_addr,1'd0}] <= 8'hfe;
      if (m_we) begin
	 {memory8_16[{m_addr,1'd1}],
	  memory8_16[{m_addr,1'd0}]} <= m_data;
      end
   end
   
   always @ (posedge clk) begin
      m_we <= 0;
      if (cyc!=0) begin
	 cyc <= cyc + 1;
	 if (cyc==1) begin
	    m_we <= 1'b1;
	    m_addr <= 3'd2;
	    m_data <= 16'h55_44;
	 end
	 if (cyc==2) begin
	    m_we <= 1'b1;
	    m_addr <= 3'd3;
	    m_data <= 16'h77_66;
	 end
	 if (cyc==3) begin
	    m_we <= 0;	// Check we really don't write this
	    m_addr <= 3'd3;
	    m_data <= 16'h0bad;
	 end
	 if (cyc==5) begin
	    if (memory8_16[4] != 8'h44) $stop;
	    if (memory8_16[5] != 8'h55) $stop;
	    if (memory8_16[6] != 8'hfe) $stop;
	    if (memory8_16[7] != 8'h77) $stop;
	    $write("*-* All Finished *-*\n");
	    $finish;
	 end
      end
   end

endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
