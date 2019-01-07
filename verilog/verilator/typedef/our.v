`define EXPOSZ 2

typedef struct packed {
    bit [4:0]   expo;
    bit         sign;
    bit [1:0]   mant;
} FP;

typedef struct packed {
    reg [4:0]  expo;
    reg        sign;
    reg [1:0]  mant;
} FP_reg;

module our (
  // system signals
  input wire clk,
  input wire rst,
  output     FP fp
);

FP  fp_w;
FP  fp_i;
FP_reg  fp_r;

assign fp = fp_r;

always @ (posedge clk, posedge rst)
  if (rst)
    fp_r <= {$size(fp_r){1'b0}};
  else
    begin
       $display("Assign");
       fp_r <= fp_i;
    end

always @ (*)
  begin
     FP fp_v = fp_r;
     fp_v = '{ fp_v.expo+1, 1'b0, 2'b0 };
     fp_i = fp_v;
     $display(fp_i);
  end

endmodule
