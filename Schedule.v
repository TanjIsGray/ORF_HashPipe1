`timescale 1ns / 10ps
//////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023, Avant-Gray LLC.  All rights reserved.
//
// Company: Avant-Gray LLC
// Engineer: Tanj Bennett
// 
// Create Date: 09/30/2023 01:23:20 PM
// Design Name: HashPipe
// File Name: Schedule
// Project Name: https://github.com/TanjIsGray/LowE_BTC
// 
//  The base algorithm is described in FIPS 180-4:
//  https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
//
//////////////////////////////////////////////////////////////////////////////////

/// rotate a word
///
module SHR #(
        parameter SHIFT = 64,                // 64 default should never be used
        parameter WORDBITS  = 32
    ) (
        output wire [WORDBITS-1:0] y_out,
        input  wire [WORDBITS-1:0] x_in
    );
    assign y_out[WORDBITS - 1 - SHIFT : 0]  = x_in[WORDBITS - 1 : SHIFT];
    assign y_out[WORDBITS - 1 : WORDBITS - SHIFT] = {SHIFT{1'b0}};
endmodule

/// rotate a word
///
module ROTR #(
        parameter SHIFT = 64,                // 64 default should never be used
        parameter WORDBITS  = 32
    ) (
        output wire [WORDBITS-1:0] y_out,
        input  wire [WORDBITS-1:0] x_in
    );
    assign y_out = { x_in[SHIFT - 1 : 0], x_in[WORDBITS - 1 : SHIFT] };
endmodule

/// σ0 operation from SHA-256
///
module sig0 #(
        parameter WORDBITS  = 32
    ) (
        output wire [WORDBITS-1:0] y_out,
        input  wire [WORDBITS-1:0] x_in
    );
//  uint32_t σ0(uint32_t x) { return ROTR(x, 7) ^ ROTR(x, 18) ^ (x >> 3); }
  
    wire [WORDBITS-1:0] a, b, c;
    
    ROTR #(.SHIFT(7))  rr7  (a, x_in);
    ROTR #(.SHIFT(18)) rr18 (b, x_in);
    SHR  #(.SHIFT(3))  sr3  (c, x_in);
    
    assign y_out = a ^ b ^ c;
endmodule

/// σ1 operation from SHA-256
///
module sig1 #(
        parameter WORDBITS  = 32
    ) (
        output wire [WORDBITS-1:0] y_out,
        input  wire [WORDBITS-1:0] x_in
    );
//  uint32_t σ1(uint32_t x) { return ROTR(x, 17) ^ ROTR(x, 19) ^ (x >> 10); }
  
    wire [WORDBITS-1:0] a, b, c;
    
    ROTR #(.SHIFT(17)) rr17 (a, x_in);
    ROTR #(.SHIFT(19)) rr19 (b, x_in);
    SHR  #(.SHIFT(10)) sr10 (c, x_in);
    
    assign y_out = a ^ b ^ c;
endmodule

/// The message scheduling is at FIPS 180-4, §6.2.2.1
/// 4 stages are fused here into one.
///
module ScheduleMessageQuad #(
        parameter WORDBITS  = 32,
        parameter MSGWORDS  = 16,       // message width in words
        parameter MSGBITS   = MSGWORDS * WORDBITS,
        parameter [WORDBITS-1:0]    K0_constIn = 0,
        parameter [WORDBITS-1:0]    K1_constIn = 0,
        parameter [WORDBITS-1:0]    K2_constIn = 0,
        parameter [WORDBITS-1:0]    K3_constIn = 0
    ) (
        output wire [WORDBITS-1:0]  KW0_out,
        output wire [WORDBITS-1:0]  KW1_out,
        output wire [WORDBITS-1:0]  KW2_out,
        output wire [WORDBITS-1:0]  KW3_out,
        output wire [MSGBITS-1:0]   W_out,
        input                       clk,
        input  wire [MSGBITS-1 : 0] W_in
    );

    wire [WORDBITS-1:0] W [MSGWORDS-1: 0];
    
    // by convention, all stages synch at input, not at output
    // capture the input at rising clock
    
    generate
      genvar I;
      for (I = 0; I < MSGWORDS; I = I + 1)
      begin
        localparam IW = I * WORDBITS;
        
        PassLatch #(.WIDTH(WORDBITS)) inlatch (.q(W[I]), .clk(clk), .d(W_in[IW + WORDBITS - 1 : IW]));
      end
    endgenerate
  
    // mix the message schedule W[] in a cyclic recurrence.
    //   W[i & 15u] += σ1(W[(i - 2) & 15u]) + W[(i - 7) & 15u] + σ0(W[(i - 15) & 15u]);

    assign KW0_out = K0_constIn + W[0];
    assign KW1_out = K1_constIn + W[1];
    assign KW2_out = K2_constIn + W[2];
    assign KW3_out = K3_constIn + W[3];
    
    wire [WORDBITS-1:0] s0_1, s1_14, WC;
    sig0 sig0_1  (.y_out(s0_1),  .x_in(W[1]));
    sig1 sig1_14 (.y_out(s1_14), .x_in(W[14]));
    assign WC = (W[0] + W[9]) + s1_14 + s0_1;

    wire [WORDBITS-1:0] s0_2, s1_15, WD;
    sig0 sig0_2  (.y_out(s0_2),  .x_in(W[2]));
    sig1 sig1_15 (.y_out(s1_15), .x_in(W[15]));
    assign WD = (W[1] + W[10]) + s1_15 + s0_2;

    wire [WORDBITS-1:0] s0_3, s1_00, WE;
    sig0 sig0_3  (.y_out(s0_3),  .x_in(W[3]));
    sig1 sig1_00 (.y_out(s1_00), .x_in(WC));
    assign WE = (W[2] + W[11]) + s1_00 + s0_3;

    wire [WORDBITS-1:0] s0_4, s1_01, WF;
    sig0 sig0_4  (.y_out(s0_4),  .x_in(W[4]));
    sig1 sig1_01 (.y_out(s1_01), .x_in(WD));
    assign WF = (W[3] + W[12]) + s1_01 + s0_4;

    localparam CW = 12 * WORDBITS;
    localparam DW = 13 * WORDBITS;
    localparam EW = 14 * WORDBITS;
    localparam FW = 15 * WORDBITS;

    assign W_out[CW + WORDBITS - 1 : CW] = WC;
    assign W_out[DW + WORDBITS - 1 : DW] = WD;
    assign W_out[EW + WORDBITS - 1 : EW] = WE;
    assign W_out[FW + WORDBITS - 1 : FW] = WF;

    generate
      genvar J;
      for (J = 0; J < (MSGWORDS - 4); J = J + 1)
      begin
        localparam JW = J * WORDBITS;
        assign W_out[JW + WORDBITS - 1 : JW] = W[J + 4];
      end
    endgenerate

endmodule

/// The message scheduling is at FIPS 180-4, §6.2.2.1
/// 4 stages are fused here into one.  The final quads just buffer previous calculations.
///
module ScheduleMessageFinalQuads #(
        parameter WORDBITS  = 32,
        parameter WINWORDS  = 16,               // input message width in words
        parameter WOUTWORDS = (WINWORDS - 4),   // 4 words consumed in each of the final quads
        parameter [WORDBITS-1:0]    K0_constIn = 0,
        parameter [WORDBITS-1:0]    K1_constIn = 0,
        parameter [WORDBITS-1:0]    K2_constIn = 0,
        parameter [WORDBITS-1:0]    K3_constIn = 0
    ) (
        output wire [WORDBITS-1:0]  KW0_out,
        output wire [WORDBITS-1:0]  KW1_out,
        output wire [WORDBITS-1:0]  KW2_out,
        output wire [WORDBITS-1:0]  KW3_out,
        output wire [(WOUTWORDS * WORDBITS)-1:0]   W_out,
        input                       clk,
        input  wire [(WINWORDS * WORDBITS)-1 : 0] W_in
    );

    wire [WORDBITS-1:0] W [WINWORDS-1: 0];
    
    // by convention, all stages synch at input, not at output
    // capture the input at rising clock
    
    generate
        genvar I;
        for (I = 0; I < WINWORDS; I = I + 1)
        begin
            localparam IW = I * WORDBITS;
            PassLatch #(.WIDTH(WORDBITS)) inlatch (.q(W[I]), .clk(clk), .d(W_in[IW + WORDBITS - 1 : IW]));
        end
    endgenerate
  
    // mix the message schedule W[] in a cyclic recurrence.
    //   W[i & 15u] += σ1(W[(i - 2) & 15u]) + W[(i - 7) & 15u] + σ0(W[(i - 15) & 15u]);

    assign KW0_out = K0_constIn + W[0];
    assign KW1_out = K1_constIn + W[1];
    assign KW2_out = K2_constIn + W[2];
    assign KW3_out = K3_constIn + W[3];

    generate
        genvar J;
        for (J = 0; J < (WOUTWORDS - 4); J = J + 1)
        begin
            localparam JW = J * WORDBITS;
            assign W_out[JW + WORDBITS - 1 : JW] = W[J + 4];
        end
    endgenerate

endmodule

/// The message scheduling is at FIPS 180-4, §6.2.2.1
/// 4 stages are fused here into one.  The final quads just buffer previous calculations.
///
module ScheduleMessageQuad15 #(
        parameter WORDBITS  = 32,
        parameter WINWORDS  = 4,               // input message width in words
        parameter [WORDBITS-1:0]    K0_constIn = 0,
        parameter [WORDBITS-1:0]    K1_constIn = 0,
        parameter [WORDBITS-1:0]    K2_constIn = 0,
        parameter [WORDBITS-1:0]    K3_constIn = 0
    ) (
        output wire [WORDBITS-1:0]  KW0_out,
        output wire [WORDBITS-1:0]  KW1_out,
        output wire [WORDBITS-1:0]  KW2_out,
        output wire [WORDBITS-1:0]  KW3_out,
        input                       clk,
        input  wire [(WINWORDS * WORDBITS)-1 : 0] W_in
    );

    wire [WORDBITS-1:0] W [WINWORDS-1: 0];
    
    // by convention, all stages synch at input, not at output
    // capture the input at rising clock
    
    generate
        genvar I;
        for (I = 0; I < WINWORDS; I = I + 1)
        begin
            localparam IW = I * WORDBITS;
            PassLatch #(.WIDTH(WORDBITS)) inlatch (.q(W[I]), .clk(clk), .d(W_in[IW + WORDBITS - 1 : IW]));
        end
    endgenerate
  
    // mix the message schedule W[] in a cyclic recurrence.
    //   W[i & 15u] += σ1(W[(i - 2) & 15u]) + W[(i - 7) & 15u] + σ0(W[(i - 15) & 15u]);

    assign KW0_out = K0_constIn + W[0];
    assign KW1_out = K1_constIn + W[1];
    assign KW2_out = K2_constIn + W[2];
    assign KW3_out = K3_constIn + W[3];

endmodule

