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
  // Lib.hs:18:1-36
  wire [8:0] c$ds_app_arg;
  // Puzzle.hs:(11,1)-(12,67)
  reg [58:0] maybeToBoola_c$ds_app_arg = {2'd0,   {14'd0,   14'd0,   14'd0,   14'd0},   1'b0};
  wire [33:0] result_0;
  wire [92:0] result_1;
  reg [33:0] c$app_arg;
  wire [33:0] c$case_alt;
  wire [31:0] c$app_arg_0;
  wire [127:0] c$app_arg_1;
  // Puzzle.hs:39:1-11
  reg  \done' ;
  reg [55:0] c$app_arg_2;
  wire [55:0] c$case_alt_0;
  wire [55:0] result_2;
  wire signed [63:0] c$app_arg_3;
  // Puzzle.hs:39:1-11
  wire signed [63:0] wild3;
  // Puzzle.hs:39:1-11
  wire [55:0] numbers1;
  // Puzzle.hs:39:1-11
  wire signed [63:0] c$wild3_app_arg;
  reg [1:0] c$app_arg_4;
  // Puzzle.hs:39:1-11
  wire [1:0] action;
  wire [1:0] c$case_alt_1;
  // Puzzle.hs:39:1-11
  wire [1:0] digitIdx1;
  // Puzzle.hs:39:1-11
  wire  \c$done'_case_alt ;
  // Puzzle.hs:39:1-11
  reg  \c$done'_case_alt_0 ;
  reg [1:0] result_3;
  // Puzzle.hs:39:1-11
  wire [7:0] a1;
  // Puzzle.hs:(15,1)-(16,69)
  reg [97:0] maybeToBoola_c$ds_app_arg_0 = {32'd0,   {2'b00,32'd4294967295,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}};
  wire [32:0] result_4;
  wire [130:0] c$case_alt_2;
  reg [32:0] c$app_arg_5;
  reg [65:0] c$app_arg_6;
  wire [65:0] c$case_alt_3;
  wire [65:0] c$case_alt_4;
  wire [65:0] c$case_alt_5;
  wire [65:0] c$case_alt_6;
  // Puzzle.hs:93:1-11
  wire [31:0] inp;
  // Puzzle.hs:93:1-11
  wire [31:0] prev;
  reg [31:0] c$app_arg_7;
  wire [31:0] c$case_alt_7;
  // Puzzle.hs:93:1-11
  wire [31:0] count;
  // Puzzle.hs:93:1-11
  wire [31:0] prev_0;
  // Puzzle.hs:93:1-11
  wire [31:0] \input ;
  // Puzzle.hs:93:1-11
  wire [65:0] phase;
  // Puzzle.hs:(19,1)-(20,68)
  reg [35:0] maybeToBoola_c$ds_app_arg_1 = {{1'b0,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx},   3'd0};
  wire [8:0] result_5;
  wire [44:0] result_6;
  wire [8:0] c$app_arg_8;
  wire [8:0] c$case_alt_8;
  wire signed [63:0] c$app_arg_9;
  wire [2:0] c$app_arg_10;
  wire [3:0] z;
  wire [2:0] result_7;
  wire [32:0] c$app_arg_11;
  wire [31:0] c$app_arg_12;
  // Puzzle.hs:125:1-15
  wire [31:0] divider;
  // Puzzle.hs:125:1-15
  wire [31:0] s;
  // Puzzle.hs:125:1-15
  wire [32:0] number;
  wire [9:0] result_8;
  // Puzzle.hs:125:1-15
  wire signed [63:0] wild3_0;
  // Puzzle.hs:125:1-15
  wire signed [63:0] c$wild3_app_arg_0;
  // Puzzle.hs:125:1-15
  wire [2:0] digitIdx1_0;
  // Lib.hs:(22,1)-(23,72)
  reg [265:0] maybeToBoola_c$ds_app_arg_2 = {{8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
  8'b00000001,   8'b00000001},   5'd0,   5'd0};
  wire [8:0] result_9;
  // Lib.hs:40:1-15
  wire signed [63:0] c$wild3_app_arg_1;
  wire [255:0] result_10;
  wire [255:0] c$app_arg_13;
  wire [4:0] c$app_arg_14;
  wire [4:0] c$app_arg_15;
  // Lib.hs:40:1-15
  wire signed [63:0] c$wild3_app_arg_2;
  wire [7:0] result_11;
  wire [8:0] c$app_arg_16;
  wire [274:0] result_12;
  // Lib.hs:40:1-15
  wire  canSend;
  // Lib.hs:40:1-15
  wire [7:0] a;
  // Lib.hs:40:1-15
  wire signed [63:0] wild3_1;
  // Lib.hs:40:1-15
  wire signed [63:0] wild3_2;
  // Lib.hs:40:1-15
  wire  doSend;
  // Lib.hs:40:1-15
  wire [4:0] readPointer1;
  // Lib.hs:40:1-15
  wire [4:0] writePointer1;
  // Lib.hs:40:1-15
  wire [255:0] memory1;
  // Lib.hs:18:1-36
  wire [7:0] a1_0;
  wire [9:0] eta;
  wire [55:0] c$vec;
  wire [7:0] c$bv;
  wire [3:0] result_selection_res;
  wire [7:0] c$bv_0;
  wire [32:0] c$case_alt_selection_6;
  wire [32:0] c$case_alt_selection_9;
  wire [31:0] c$i_27;
  wire [39:0] c$vecFlat;
  wire [8:0] result;

  assign eta = {can_send,   data_in,   in_stb};

  assign c$ds_app_arg = eta[0:0] ? {1'b1,eta[8:1]} : {1'b0,8'bxxxxxxxx};

  // register begin
  always @(posedge clk or  posedge  reset) begin : maybeToBoola_c$ds_app_arg_register
    if ( reset) begin
      maybeToBoola_c$ds_app_arg <= {2'd0,   {14'd0,   14'd0,   14'd0,   14'd0},   1'b0};
    end else if (enable) begin
      maybeToBoola_c$ds_app_arg <= result_1[92:34];
    end
  end
  // register end

  assign result_0 = result_1[33:0];

  assign result_1 = {{c$app_arg_4,   c$app_arg_2,
                      \done' },   c$app_arg};

  always @(*) begin
    case(action)
      2'b00 : c$app_arg = c$case_alt;
      default : c$app_arg = {1'b0,33'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
    endcase
  end

  assign c$case_alt = \done'  ? {1'b1,{1'b1,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}} : {1'b1,{1'b0,c$app_arg_0}};

  wire [127:0] vec;
  wire [31:0] acc_2_0;
  wire [31:0] acc_1;
  wire [31:0] acc_2;
  wire [31:0] acc_3;
  wire [31:0] acc_4;
  wire [31:0] acc_1_0;
  wire [31:0] acc_1_1;
  assign c$app_arg_0 = acc_2_0;

  assign vec = c$app_arg_1;

  assign acc_1 = vec[127:96];

  assign acc_2 = vec[95:64];

  assign acc_3 = vec[63:32];

  assign acc_4 = vec[31:0];

  assign acc_1_0 = acc_1 + acc_2;



  assign acc_1_1 = acc_3 + acc_4;



  assign acc_2_0 = acc_1_0 + acc_1_1;





  // map begin
  genvar i;
  generate
  for (i=0; i < 4; i = i + 1) begin : map
    wire [13:0] map_in;
    assign map_in = numbers1[i*14+:14];
    wire [31:0] map_out;
    assign map_out = {{(32-14) {1'b0}},map_in};


    assign c$app_arg_1[i*32+:32] = map_out;
  end
  endgenerate
  // map end

  always @(*) begin
    case(digitIdx1)
      2'd0 : \done'  = \c$done'_case_alt ;
      default : \done'  = maybeToBoola_c$ds_app_arg[0:0];
    endcase
  end

  always @(*) begin
    case(action)
      2'b00 : c$app_arg_2 = {4 {14'd0}};
      default : c$app_arg_2 = c$case_alt_0;
    endcase
  end

  assign c$case_alt_0 = c$ds_app_arg[8:8] ? result_2 : numbers1;

  assign c$vec = {numbers1[55:42] * 14'd10,
                  numbers1[41:28] * 14'd10,
                  numbers1[27:14] * 14'd10,
                  numbers1[13:0] * 14'd10};

  // vector replace begin
  genvar i_0;
  generate
  for (i_0=0;i_0<4;i_0=i_0+1) begin : vector_replace
    assign result_2[(3-i_0)*14+:14] = (wild3) == i_0 ? ($unsigned(c$app_arg_3[0+:14])) : c$vec[(3-i_0)*14+:14];
  end
  endgenerate
  // vector replace end

  assign c$bv = (a1 & 8'b00001111);

  assign c$app_arg_3 = $unsigned({{(64-8) {1'b0}},c$bv});

  assign wild3 = $signed(c$wild3_app_arg);

  assign numbers1 = maybeToBoola_c$ds_app_arg[56:1];

  assign c$wild3_app_arg = $unsigned({{(64-2) {1'b0}},digitIdx1});

  always @(*) begin
    case(action)
      2'b01 : c$app_arg_4 = c$case_alt_1;
      2'b11 : c$app_arg_4 = digitIdx1;
      default : c$app_arg_4 = 2'd0;
    endcase
  end

  assign action = c$ds_app_arg[8:8] ? result_3 : 2'd3;

  assign c$case_alt_1 = (digitIdx1 < 2'd3) ? (digitIdx1 + 2'd1) : digitIdx1;

  assign digitIdx1 = maybeToBoola_c$ds_app_arg[58:57];

  assign \c$done'_case_alt  = c$ds_app_arg[8:8] ? \c$done'_case_alt_0  : maybeToBoola_c$ds_app_arg[0:0];

  always @(*) begin
    case(a1)
      8'b00001010 : \c$done'_case_alt_0  = 1'b1;
      default : \c$done'_case_alt_0  = maybeToBoola_c$ds_app_arg[0:0];
    endcase
  end

  assign c$bv_0 = (a1 >> (64'sd4));

  assign result_selection_res = c$bv_0[0+:4];

  always @(*) begin
    case(result_selection_res)
      4'b0011 : result_3 = 2'd1;
      4'b0000 : result_3 = 2'd0;
      default : result_3 = 2'd2;
    endcase
  end

  assign a1 = c$ds_app_arg[7:0];

  // register begin
  always @(posedge clk or  posedge  reset) begin : maybeToBoola_c$ds_app_arg_0_register
    if ( reset) begin
      maybeToBoola_c$ds_app_arg_0 <= {32'd0,   {2'b00,32'd4294967295,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx}};
    end else if (enable) begin
      maybeToBoola_c$ds_app_arg_0 <= c$case_alt_2[130:33];
    end
  end
  // register end

  assign result_4 = c$case_alt_2[32:0];

  assign c$case_alt_2 = {{c$app_arg_7,
                          c$app_arg_6},   c$app_arg_5};

  always @(*) begin
    case(phase[65:64])
      2'b10 : c$app_arg_5 = {1'b1,count};
      default : c$app_arg_5 = {1'b0,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
    endcase
  end

  always @(*) begin
    case(phase[65:64])
      2'b00 : c$app_arg_6 = c$case_alt_4;
      2'b01 : c$app_arg_6 = c$case_alt_3;
      2'b10 : c$app_arg_6 = {2'b11,64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
      default : c$app_arg_6 = {2'b11,64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
    endcase
  end

  assign c$case_alt_3 = result_0[33:33] ? c$case_alt_5 : {2'b00,\input ,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};

  assign c$case_alt_4 = result_0[33:33] ? c$case_alt_6 : phase;

  assign c$case_alt_selection_6 = result_0[32:0];

  assign c$case_alt_5 = c$case_alt_selection_6[32:32] ? {2'b10,64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx} : {2'b01,inp,\input };

  assign c$case_alt_selection_9 = result_0[32:0];

  assign c$case_alt_6 = c$case_alt_selection_9[32:32] ? {2'b10,64'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx} : {2'b01,inp,prev};

  assign inp = result_0[31:0];

  assign prev = phase[63:32];

  always @(*) begin
    case(phase[65:64])
      2'b01 : c$app_arg_7 = c$case_alt_7;
      2'b10 : c$app_arg_7 = {32 {1'bx}};
      default : c$app_arg_7 = count;
    endcase
  end

  assign c$case_alt_7 = (\input  > prev_0) ? (count + 32'd1) : count;

  assign count = maybeToBoola_c$ds_app_arg_0[97:66];

  assign prev_0 = phase[31:0];

  assign \input  = phase[63:32];

  assign phase = maybeToBoola_c$ds_app_arg_0[65:0];

  // register begin
  always @(posedge clk or  posedge  reset) begin : maybeToBoola_c$ds_app_arg_1_register
    if ( reset) begin
      maybeToBoola_c$ds_app_arg_1 <= {{1'b0,32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx},   3'd0};
    end else if (enable) begin
      maybeToBoola_c$ds_app_arg_1 <= result_6[44:9];
    end
  end
  // register end

  assign result_5 = result_6[8:0];

  assign result_6 = {{c$app_arg_11,
                      c$app_arg_10},   c$app_arg_8};

  assign c$app_arg_8 = (digitIdx1_0 < 3'd4) ? c$case_alt_8 : {1'b0,8'bxxxxxxxx};

  assign c$case_alt_8 = number[32:32] ? {1'b1,$unsigned(c$app_arg_9[0+:8])} : {1'b0,8'bxxxxxxxx};

  assign c$i_27 = (32'd48 | c$app_arg_12);

  assign c$app_arg_9 = $unsigned({{(64-32) {1'b0}},c$i_27});

  assign c$app_arg_10 = number[32:32] ? result_7 : 3'd0;

  assign z = digitIdx1_0 + 3'd1;

  assign result_7 = (z > 4'd4) ? 3'd4 : (z[0+:3]);

  assign c$app_arg_11 = number[32:32] ? {1'b1,s - (c$app_arg_12 * divider)} : result_4;

  assign c$app_arg_12 = s / divider;

  assign divider = {{(32-10) {1'b0}},result_8};

  assign s = number[31:0];

  assign number = maybeToBoola_c$ds_app_arg_1[35:3];

  assign c$vecFlat = {10'd1000,   10'd100,
                      10'd10,   10'd1};

  // index begin
  wire [9:0] vecArray [0:4-1];
  genvar i_1;
  generate
  for (i_1=0; i_1 < 4; i_1=i_1+1) begin : mk_array
    assign vecArray[(4-1)-i_1] = c$vecFlat[i_1*10+:10];
  end
  endgenerate
  assign result_8 = vecArray[(wild3_0)];
  // index end

  assign wild3_0 = $signed(c$wild3_app_arg_0);

  assign c$wild3_app_arg_0 = $unsigned({{(64-3) {1'b0}},digitIdx1_0});

  assign digitIdx1_0 = maybeToBoola_c$ds_app_arg_1[2:0];

  // register begin
  always @(posedge clk or  posedge  reset) begin : maybeToBoola_c$ds_app_arg_2_register
    if ( reset) begin
      maybeToBoola_c$ds_app_arg_2 <= {{8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,   8'b00000001,
    8'b00000001,   8'b00000001},   5'd0,   5'd0};
    end else if (enable) begin
      maybeToBoola_c$ds_app_arg_2 <= result_12[274:9];
    end
  end
  // register end

  assign result_9 = result_12[8:0];

  assign c$wild3_app_arg_1 = $unsigned({{(64-5) {1'b0}},writePointer1});

  // vector replace begin
  genvar i_2;
  generate
  for (i_2=0;i_2<32;i_2=i_2+1) begin : vector_replace_0
    assign result_10[(31-i_2)*8+:8] = (wild3_1) == i_2 ? a : memory1[(31-i_2)*8+:8];
  end
  endgenerate
  // vector replace end

  assign c$app_arg_13 = result_5[8:8] ? result_10 : memory1;

  assign c$app_arg_14 = result_5[8:8] ? (writePointer1 + 5'd1) : writePointer1;

  assign c$app_arg_15 = doSend ? (readPointer1 + 5'd1) : readPointer1;

  assign c$wild3_app_arg_2 = $unsigned({{(64-5) {1'b0}},readPointer1});

  // index begin
  wire [7:0] vecArray_0 [0:32-1];
  genvar i_3;
  generate
  for (i_3=0; i_3 < 32; i_3=i_3+1) begin : mk_array_0
    assign vecArray_0[(32-1)-i_3] = memory1[i_3*8+:8];
  end
  endgenerate
  assign result_11 = vecArray_0[(wild3_2)];
  // index end

  assign c$app_arg_16 = doSend ? {1'b1,result_11} : {1'b0,8'bxxxxxxxx};

  assign result_12 = {{c$app_arg_13,
                       c$app_arg_14,   c$app_arg_15},   c$app_arg_16};

  assign canSend = eta[9:9];

  assign a = result_5[7:0];

  assign wild3_1 = $signed(c$wild3_app_arg_1);

  assign wild3_2 = $signed(c$wild3_app_arg_2);

  assign doSend = canSend ? (writePointer1 > readPointer1) : 1'b0;

  assign readPointer1 = maybeToBoola_c$ds_app_arg_2[4:0];

  assign writePointer1 = maybeToBoola_c$ds_app_arg_2[9:5];

  assign memory1 = maybeToBoola_c$ds_app_arg_2[265:10];

  assign result = result_9[8:8] ? {a1_0,
                                   1'b1} : {{8 {1'bx}},   1'b0};

  assign a1_0 = result_9[7:0];

  assign data_out = result[8:1];

  assign out_stb = result[0:0];


endmodule

