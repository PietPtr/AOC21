/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module serial
    ( // Inputs
      input  clk // clock
    , input  reset // reset
    , input  enable // enable
    , input  can_send
    , input [7:0] data_in
    , input  in_stb

      // Outputs
    , output wire [7:0] data_out
    , output wire  out_stb
    );
  // Types.hs:17:1-36
  wire [8:0] c$ds_app_arg;
  // Puzzle.hs:(7,1)-(8,67)
  wire signed [63:0] c$ds_app_arg_0;
  // Puzzle.hs:(7,1)-(8,67)
  wire [32:0] c$ds_app_arg_1;
  // Puzzle.hs:32:1-11
  wire [7:0] a1;
  // Puzzle.hs:(11,1)-(12,69)
  wire [33:0] c$ds_case_alt;
  wire [33:0] result_0;
  wire [32:0] result_1;
  // Puzzle.hs:(11,1)-(12,69)
  reg  maybeToBoola_sending = 1'b0;
  wire signed [63:0] c$app_arg;
  wire [8:0] result_2;
  // Types.hs:(21,1)-(22,72)
  reg [265:0] maybeToBoola_c$ds_app_arg = {{8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001},   5'd0,   5'd0};
  wire [8:0] result_3;
  // Types.hs:39:1-15
  wire signed [63:0] c$wild3_app_arg;
  wire [255:0] result_4;
  wire [255:0] c$app_arg_0;
  wire [4:0] c$app_arg_1;
  wire [4:0] c$app_arg_2;
  // Types.hs:39:1-15
  wire signed [63:0] c$wild3_app_arg_0;
  wire [7:0] result_5;
  wire [8:0] c$app_arg_3;
  wire [274:0] result_6;
  // Types.hs:39:1-15
  wire  canSend;
  // Types.hs:39:1-15
  wire [7:0] a;
  // Types.hs:39:1-15
  wire signed [63:0] wild3;
  // Types.hs:39:1-15
  wire signed [63:0] wild3_0;
  // Types.hs:39:1-15
  wire  doSend;
  // Types.hs:39:1-15
  wire [4:0] readPointer1;
  // Types.hs:39:1-15
  wire [4:0] writePointer1;
  // Types.hs:39:1-15
  wire [255:0] memory1;
  // Puzzle.hs:(24,1)-(25,68)
  wire [31:0] a1_0;
  // Types.hs:17:1-36
  wire [7:0] a1_1;
  wire [9:0] eta;
  wire [8:0] result;

  assign eta = {can_send,   data_in,   in_stb};

  assign c$ds_app_arg = eta[0:0] ? {1'b1,eta[8:1]} : {1'b0,8'bxxxxxxxx};

  assign c$ds_app_arg_0 = $unsigned({{(64-8) {1'b0}},a1});

  assign c$ds_app_arg_1 = c$ds_app_arg[8:8] ? {1'b1,$unsigned(c$ds_app_arg_0[0+:32])} : {1'b0,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};

  assign a1 = c$ds_app_arg[7:0];

  assign c$ds_case_alt = c$ds_app_arg_1[32:32] ? {1'b1,
                                                  {1'b1,32'd65}} : {1'b0,
                                                                    {1'b0,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}};

  assign result_0 = maybeToBoola_sending ? {1'b1,
                                            {1'b1,32'd66}} : c$ds_case_alt;

  assign result_1 = result_0[32:0];

  // register begin
  always @(posedge clk or  posedge  reset) begin : maybeToBoola_sending_register
    if ( reset) begin
      maybeToBoola_sending <= 1'b0;
    end else if (enable) begin
      maybeToBoola_sending <= result_0[33:33];
    end
  end
  // register end

  assign c$app_arg = $unsigned({{(64-32) {1'b0}},a1_0});

  assign result_2 = result_1[32:32] ? {1'b1,$unsigned(c$app_arg[0+:8])} : {1'b0,8'bxxxxxxxx};

  // register begin
  always @(posedge clk or  posedge  reset) begin : maybeToBoola_c$ds_app_arg_register
    if ( reset) begin
      maybeToBoola_c$ds_app_arg <= {{8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001},   5'd0,   5'd0};
    end else if (enable) begin
      maybeToBoola_c$ds_app_arg <= result_6[274:9];
    end
  end
  // register end

  assign result_3 = result_6[8:0];

  assign c$wild3_app_arg = $unsigned({{(64-5) {1'b0}},writePointer1});

  // vector replace begin
  genvar i;
  generate
  for (i=0;i<32;i=i+1) begin : vector_replace
    assign result_4[(31-i)*8+:8] = (wild3) == i ? a : memory1[(31-i)*8+:8];
  end
  endgenerate
  // vector replace end

  assign c$app_arg_0 = result_2[8:8] ? result_4 : memory1;

  assign c$app_arg_1 = result_2[8:8] ? (writePointer1 + 5'd1) : writePointer1;

  assign c$app_arg_2 = doSend ? (readPointer1 + 5'd1) : readPointer1;

  assign c$wild3_app_arg_0 = $unsigned({{(64-5) {1'b0}},readPointer1});

  // index begin
  wire [7:0] vecArray [0:32-1];
  genvar i_0;
  generate
  for (i_0=0; i_0 < 32; i_0=i_0+1) begin : mk_array
    assign vecArray[(32-1)-i_0] = memory1[i_0*8+:8];
  end
  endgenerate
  assign result_5 = vecArray[(wild3_0)];
  // index end

  assign c$app_arg_3 = doSend ? {1'b1,result_5} : {1'b0,8'bxxxxxxxx};

  assign result_6 = {{c$app_arg_0,   c$app_arg_1,
                      c$app_arg_2},   c$app_arg_3};

  assign canSend = eta[9:9];

  assign a = result_2[7:0];

  assign wild3 = $signed(c$wild3_app_arg);

  assign wild3_0 = $signed(c$wild3_app_arg_0);

  assign doSend = canSend ? (writePointer1 > readPointer1) : 1'b0;

  assign readPointer1 = maybeToBoola_c$ds_app_arg[4:0];

  assign writePointer1 = maybeToBoola_c$ds_app_arg[9:5];

  assign memory1 = maybeToBoola_c$ds_app_arg[265:10];

  assign a1_0 = result_1[31:0];

  assign result = result_3[8:8] ? {a1_1,
                                   1'b1} : {{8 {1'bx}},   1'b0};

  assign a1_1 = result_3[7:0];

  assign data_out = result[8:1];

  assign out_stb = result[0:0];


endmodule
