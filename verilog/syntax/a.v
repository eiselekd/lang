module a (input  clk,
	  output out_C,
	  output out_D );

   assign out_C = C;
   assign out_D = D;

   reg A, B;
   wire C, D;

   my_module #(2, 4, 3) m1 (clk, B, C, D);

   // x = 2, y = 4, z = 3 in instance m1
   my_module #(5, 3, 1) m2 (.b(B), .d(D), .c(C), .a(clk));

   // x = 5, y = 3, z = 1 in instance m2
   defparam m3.x = 4, m3.y = 2, m3.z = 5;
   my_module m3 (clk, B, C, D); // x = 4, y = 2, z = 5 in instance m3

endmodule
