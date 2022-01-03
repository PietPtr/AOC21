/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.2.5. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module serial
    ( // Inputs
      input  clk // clock
    , input  reset // reset
    , input  enable
    , input  cansend

      // Outputs
    , output wire [8:0] out
    );
  // Serial.hs:25:1-32
  reg [4:0] c$ds_app_arg = 5'd0;
  // Serial.hs:15:1-10
  wire signed [63:0] wild;
  wire [7:0] result;
  // Serial.hs:15:1-10
  wire signed [63:0] c$wild_app_arg;
  wire [4:0] c$app_arg;
  wire [13:0] result_0;
  wire [207:0] c$vecFlat;

  // register begin
  always @(posedge clk or  posedge  reset) begin : c$ds_app_arg_register
    if ( reset) begin
      c$ds_app_arg <= 5'd0;
    end else  if (enable)  begin
      c$ds_app_arg <= result_0[13:9];
    end
  end
  // register end

  assign out = result_0[8:0];

  assign wild = $signed(c$wild_app_arg);

  assign c$vecFlat = {8'd65
                     ,8'd66
                     ,8'd67
                     ,8'd68
                     ,8'd69
                     ,8'd70
                     ,8'd71
                     ,8'd72
                     ,8'd73
                     ,8'd74
                     ,8'd75
                     ,8'd76
                     ,8'd77
                     ,8'd78
                     ,8'd79
                     ,8'd80
                     ,8'd81
                     ,8'd82
                     ,8'd83
                     ,8'd84
                     ,8'd85
                     ,8'd86
                     ,8'd87
                     ,8'd88
                     ,8'd89
                     ,8'd90};

  // index begin
  wire [7:0] vecArray [0:26-1];
  genvar i;
  generate
  for (i=0; i < 26; i=i+1) begin : mk_array
    assign vecArray[(26-1)-i] = c$vecFlat[i*8+:8];
  end
  endgenerate
  assign result = vecArray[(wild)];
  // index end

  assign c$wild_app_arg = $unsigned({{(64-5) {1'b0}},c$ds_app_arg});

  assign c$app_arg = cansend ? ((c$ds_app_arg + 5'd1) % 5'd26) : c$ds_app_arg;

  assign result_0 = {c$app_arg,{result,cansend}};


endmodule
