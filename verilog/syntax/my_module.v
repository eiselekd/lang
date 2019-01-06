module my_module ( input a,
		   input b,
		   output c,
		   output d );
   parameter x = 0, y = 0, z = 0;
   reg 		  C;
   assign c = C;

   always @ (posedge a)
     C <= ~C;

endmodule
