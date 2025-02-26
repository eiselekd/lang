module our #(
  parameter WIDTH = 8
)(
  // system signals
  input wire 		 clk,
  input wire 		 rst,
  // counter signas
  input wire 		 cen, // counter enable
  input wire 		 wen, // write enable
  input wire [WIDTH-1:0] dat, // input data
  output reg [WIDTH-1:0] o_p, // output value (posedge counter)
  output reg [WIDTH-1:0] o_n, // output value (negedge counter)

  output wire [7:0] data_o

);

reg [7:0] 		  data = 0;

assign data_o = data;


always @ (posedge clk, posedge rst)
if (rst) o_p <= {WIDTH{1'b0}};
else     begin
   o_p <= wen ? dat : o_p + {{WIDTH-1{1'b0}}, cen};
   data <= data + 1;
end

always @ (negedge clk, posedge rst)
if (rst) o_n <= {WIDTH{1'b0}};
else     o_n <= wen ? dat : o_n + {{WIDTH-1{1'b0}}, cen};


endmodule
