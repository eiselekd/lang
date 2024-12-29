// $Id$
// DESCRIPTION: Verilator: Verilog Test module
//
// This file ONLY is placed into the Public Domain, for any use,
// without warranty, 2003 by Wilson Snyder.

module t (/*AUTOARG*/
   // Outputs
   passed, 
   // Inputs
   clk, fastclk, reset_l
   );

   input clk /*verilator sc_clock*/;
   input fastclk /*verilator sc_clock*/;
   input reset_l;
   output passed;

   // Combine passed signals from each sub signal
   // verilator lint_off MULTIDRIVEN
   wire [20:0] passedv;
   // verilator lint_on MULTIDRIVEN
   wire   passed = &passedv;

   t_arith tarith
     (.passed		(passedv[0]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_case  tcase
     (.passed		(passedv[1]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   assign passedv[2] = 1'b1;
   t_equal tequal
     (.passed		(passedv[3]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   assign passedv[4] = 1'b1;
   t_initial tinitial
     (.passed		(passedv[5]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_inst  tinst
     (.passed		(passedv[6]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk),
      .fastclk				(fastclk));
   t_param tparam
     (.passed		(passedv[7]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_rnd   trnd
     (.passed		(passedv[8]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_mem   tmem
     (.passed		(passedv[9]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_blocking tblocking
     (.passed		(passedv[10]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk),
      .reset_l				(reset_l));
   t_clk tclk
     (.passed		(passedv[11]),
      /*AUTOINST*/
      // Inputs
      .fastclk				(fastclk),
      .clk				(clk),
      .reset_l				(reset_l));
   assign passedv[12] = 1'b1;
   t_func tfunc
     (.passed		(passedv[13]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_chg tchg
     (.passed		(passedv[14]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk),
      .fastclk				(fastclk));
   t_loop tloop
     (.passed		(passedv[15]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   assign passedv[16] = 1'b1;
   assign passedv[17] = 1'b1;
   assign passedv[18] = 1'b1;
   t_task ttask
     (.passed		(passedv[19]),
      /*AUTOINST*/
      // Inputs
      .clk				(clk));
   t_netlist tnetlist
     (.passed		(passedv[20]),
      .also_fastclk	(fastclk),
      /*AUTOINST*/
      // Inputs
      .fastclk				(fastclk));

endmodule

// Local Variables:
// compile-command: "./vlint __FILE__"
// End:
