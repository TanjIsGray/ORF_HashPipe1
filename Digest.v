`timescale 1ns / 10ps
//////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023, Avant-Gray LLC.  All rights reserved.
//
// Company: Avant-Gray LLC
// Engineer: Tanj Bennett
// 
// Create Date: 02/07/2022 01:23:20 PM
// Design Name: HashPipe
// File Name: Digest.sv
// Project Name: https://github.com/TanjIsGray/LowE_BTC
// 
//  The base algorithm is described in FIPS 180-4:
//  https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
//
//////////////////////////////////////////////////////////////////////////////////

/// weak pass-thru gate with a latch
///
module PassLatch #(parameter
    WIDTH             = 1
  )
  (
    output reg    [WIDTH-1:0]  q,
    input  wire   clk,
    input  wire   [WIDTH-1:0]  d
    );
    
  always @(posedge clk) begin
    q <= d;
  end
endmodule

/// weak pass-thru gate 2:1 choice of y:z controlled by x
///
module passChoice(output ch, input x, y, z);
    assign ch = x ? y : z;
endmodule

/// 2:1 choice of y:z controlled by x
///
module Choice(output chn, input x, y, z);
    wire p, q, r;
    
    assign p = ~(x & y);
    assign r = ~(~x & z);
    assign chn = ~(p & r);
endmodule

/// At least 2 out of 3 inputs.
///
module Majority(output maj, input x, y, z);
    wire p, q, r;

    assign p = ~(x & y);
    assign q = ~(x & z);
    assign r = ~(y & z);
    assign maj = ~(p & q & r);
endmodule

/// Balanced full adder not favoring carry latency
/// This should be optimized for small size and low power
///
module Add3bal(output sum, carry, input x, y, z);
    Majority c (carry, x, y, z);
    assign sum = x ^ y ^ z;
endmodule

/// Balanced half adder not favoring carry latency.
/// This shold be optimized for small size and low power
///
module Add2(output sum, carry, input x, y);
    assign carry = x & y;
    assign sum = x ^ y;
endmodule

/// Final full adder does not deliver the carry to anywhere.
/// This shold be optimized for small size and low power
///
module Add3final(output sum, input x, y, z);
    assign sum = x ^ y ^ z;
endmodule

/// First mixer has no carry inputs and fewer stages
///
module Mixer_0(
    output aSum, aCarry, dOut,
    output eCarry0, eCarry1, eSum,
    input aIn, bIn, cIn, dIn, eIn, fIn, gIn, hIn,
    input KWin, Sig0, Sig1
);
    wire Maj;
    wire Ch;
    
    // build the combinatorial inputs
    Majority majority (Maj, aIn, bIn, cIn);
    Choice choice (Ch, eIn, fIn, gIn);

    // sums and carries on the e-chain.
    wire es0;

    // the e-chain
    Add3bal ech_0 (es0, eCarry0, Ch, hIn, KWin);
    Add2    ech_1 (eSum, eCarry1, Sig1, es0);
    
    assign dOut = dIn;
    
    // sums and carries on the a-chain
    Add3bal ach_0 (aSum, aCarry, Sig0, Maj, eSum);
endmodule

/// Second mixer needs only a half-adder for the end of the a-chain
///
module Mixer_1(
    output aCarry0, aSum, aCarry,
    output eCarry0, eCarry1, eSum, eCarry,
    input aIn, bIn, cIn, dIn, eIn, fIn, gIn, hIn,
    input KWin, Sig0, Sig1,
    input eCarryIn0, eCarryIn1
);
    wire Maj;
    wire Ch;
    
    // build the combinatorial inputs
    Majority majority (Maj, aIn, bIn, cIn);
    Choice choice (Ch, eIn, fIn, gIn);

    // sums and carries on the e-chain.
    wire es0, es1;

    // the e-chain
    Add3bal ech_0 (es0, eCarry0, Ch, hIn, KWin);
    Add3bal ech_1 (es1, eCarry1, Sig1, es0, eCarryIn0);
    Add2    ech_2 (eSum, eCarry, dIn, es1);

    // sums and carries on the a-chain
    wire as0;

    // the a-chain
    Add3bal ach_0 (as0, aCarry0, Maj, es1, eCarryIn1);
    Add2    ach_1 (aSum, aCarry, Sig0, as0);
endmodule

/// Middle mixers for bits 30:2
///
module Mixer_2_30(
    output aCarry0, aSum, aCarry,
    output eCarry0, eCarry1, eSum, eCarry,
    input aIn, bIn, cIn, dIn, eIn, fIn, gIn, hIn,
    input KWin, Sig0, Sig1,
    input aCarryIn0,
    input eCarryIn0, eCarryIn1
);
    wire Maj;
    wire Ch;
    
    // build the combinatorial inputs
    Majority majority (Maj, aIn, bIn, cIn);
    Choice choice (Ch, eIn, fIn, gIn);

    // sums and carries on the e-chain.
    wire es0, es1;

    // the e-chain
    Add3bal ech_0 (es0, eCarry0, Ch, hIn, KWin);
    Add3bal ech_1 (es1, eCarry1, Sig1, es0, eCarryIn0);
    Add3bal ech_2 (eSum, eCarry, dIn, es1, eCarryIn1);

    // sums and carries on the a-chain
    wire as0;

    // the a-chain
    Add3bal ach_0 (as0, aCarry0, Maj, es1, eCarryIn1);
    Add3bal ach_1 (aSum, aCarry, Sig0, as0, aCarryIn0);
endmodule

/// final mixer has no carry outs
///
module Mixer_31(
    output aSum,
    output eSum,
    input aIn, bIn, cIn, dIn, eIn, fIn, gIn, hIn,
    input KWin, Sig0, Sig1,
    input aCarryIn0,
    input eCarryIn0, eCarryIn1
);
    wire Maj;
    wire Ch;
    
    // build the combinatorial inputs
    Majority majority (Maj, aIn, bIn, cIn);
    Choice choice (Ch, eIn, fIn, gIn);

    // sums and carries on the e-chain.
    wire es0, es1;

    // the e-chain.  Add simplifies to XOR because carries are unused.
    assign es0  = Ch ^ hIn ^ KWin;
    assign es1  = Sig1 ^ es0 ^ eCarryIn0;
    assign eSum = dIn ^ es1 ^ eCarryIn1;

    // sums and carries on the a-chain
    wire as0;

    // the a-chain.  Add simplifies to XOR because carries are unused.
    assign as0  = Maj ^ es1 ^ eCarryIn1;
    assign aSum = Sig0 ^ as0 ^ aCarryIn0;
endmodule

/// Σ0 operation from SHA-256
///
module Sigma0 #(
        parameter BIT = 64,
        parameter WORDBITS  = 32
    ) (
        output wire                 S0_out,
        input  wire [WORDBITS-1:0]  a_in
    );
        // these rotations select input bits for Sig0 function
        localparam ROT2 = (BIT + 2) & (WORDBITS-1);
        localparam ROT13 = (BIT + 13) & (WORDBITS-1);
        localparam ROT22 = (BIT + 22) & (WORDBITS-1);
        
        // build the combinatorial inputs
        assign S0_out = (a_in[ROT2]) ^ (a_in[ROT13]) ^ (a_in[ROT22]);
endmodule

/// Σ1 operation from SHA-256
///
module Sigma1 #(
        parameter BIT = 64,
        parameter WORDBITS  = 32
    ) (
        output wire                 S1_out,
        input  wire [WORDBITS-1:0]  e_in
    );
        // these rotations select bits for Sig1 function
        localparam ROT6 = (BIT + 6) & (WORDBITS-1);
        localparam ROT11 = (BIT + 11) & (WORDBITS-1);
        localparam ROT25 = (BIT + 25) & (WORDBITS-1);

        // build the combinatorial inputs
        assign S1_out = (e_in[ROT6]) ^ (e_in[ROT11]) ^ (e_in[ROT25]);
endmodule

// 32 pipes together
//
module MixAll #(
        parameter WORDBITS  = 32
    ) (
        output wire [WORDBITS-1:0]   a_out,
        output wire [WORDBITS-1:0]   e_out,
        input  wire [WORDBITS-1:0]   a_in, b_in, c_in, d_in, e_in, f_in, g_in, h_in,
        input  wire [WORDBITS-1:0]   KW_in
    );
    
    // internal nodes on the a pathway

    wire [WORDBITS-2:0] ac0;
    wire [WORDBITS-2:1] ac1;
    
    // internal nodes on the e pathway
    
    wire [WORDBITS-2:0] ec0;
    wire [WORDBITS-2:0] ec1;

    // the pipes will deliver partial sums which are combined for output
    
    wire [WORDBITS-1:0]     aSum, aCarry;
    wire [WORDBITS-1:0]     eSum, eCarry;

    assign a_out = aSum + aCarry;
    assign e_out = eSum + eCarry;
    
    // These pipes mix the output a and e bits for all 32 words.
    //   The other bits come from shifted pass through. 
    //   That output assembly wire will appear after these mixes

    genvar BIT;
    for (BIT = 0; BIT < WORDBITS; BIT = BIT + 1)
    begin
        // build the combinatorial inputs

        wire Sig0;
        wire Sig1;
        
        Sigma0 #(.BIT(BIT)) S_0 (.S0_out(Sig0), .a_in(a_in));
        Sigma1 #(.BIT(BIT)) S_1 (.S1_out(Sig1), .e_in(e_in));
        
        // each mixer works with 1 bit from each digest word

        wire aBit, bBit, cBit, dBit, eBit, fBit, gBit, hBit;
        
        assign aBit = a_in[BIT];
        assign bBit = b_in[BIT];
        assign cBit = c_in[BIT];
        assign dBit = d_in[BIT];
        assign eBit = e_in[BIT];
        assign fBit = f_in[BIT];
        assign gBit = g_in[BIT];
        assign hBit = h_in[BIT];
    
        if (0 == BIT) begin
            Mixer_0 Zero (
                .aSum(aSum[BIT]), .aCarry(aCarry[BIT+1]),
                .eCarry0(ec0[BIT]), .eCarry1(ec1[BIT]), .eSum(eSum[BIT]), .dOut(eCarry[BIT]),
                .aIn(aBit), .bIn(bBit), .cIn(cBit), .dIn(dBit),
                .eIn(eBit), .fIn(fBit), .gIn(gBit), .hIn(hBit),
                .KWin(KW_in[BIT]), .Sig0(Sig0), .Sig1(Sig1)
            );
            assign aCarry[BIT] = 0;
            assign eCarry[BIT+1] = ec1[BIT];
        end // if
        else if (1 == BIT) begin
            Mixer_1 One (
                .aCarry0(ac0[BIT]), .aSum(aSum[BIT]), .aCarry(aCarry[BIT+1]),
                .eCarry0(ec0[BIT]), .eCarry1(ec1[BIT]), .eSum(eSum[BIT]), .eCarry(eCarry[BIT+1]),
                .aIn(aBit), .bIn(bBit), .cIn(cBit), .dIn(dBit),
                .eIn(eBit), .fIn(fBit), .gIn(gBit), .hIn(hBit),
                .KWin(KW_in[BIT]), .Sig0(Sig0), .Sig1(Sig1),
                .eCarryIn0(ec0[BIT-1]), .eCarryIn1(ec1[BIT-1])
            );
        end  // if
        else if (BIT < (WORDBITS-1)) begin
            Mixer_2_30 Main(
                .aCarry0(ac0[BIT]), .aSum(aSum[BIT]), .aCarry(aCarry[BIT+1]),
                .eCarry0(ec0[BIT]), .eCarry1(ec1[BIT]), .eSum(eSum[BIT]), .eCarry(eCarry[BIT+1]),
                .aIn(aBit), .bIn(bBit), .cIn(cBit), .dIn(dBit),
                .eIn(eBit), .fIn(fBit), .gIn(gBit), .hIn(hBit),
                .KWin(KW_in[BIT]), .Sig0(Sig0), .Sig1(Sig1),
                .aCarryIn0(ac0[BIT-1]),
                .eCarryIn0(ec0[BIT-1]), .eCarryIn1(ec1[BIT-1])
                 );
            end // if
        else begin
            Mixer_31 Final (
                .aSum(aSum[BIT]),
                .eSum(eSum[BIT]),
                .aIn(aBit), .bIn(bBit), .cIn(cBit), .dIn(dBit),
                .eIn(eBit), .fIn(fBit), .gIn(gBit), .hIn(hBit),
                .KWin(KW_in[BIT]), .Sig0(Sig0), .Sig1(Sig1),
                .aCarryIn0(ac0[BIT-1]),
                .eCarryIn0(ec0[BIT-1]), .eCarryIn1(ec1[BIT-1])
                );
            end // else
    
    end // for BIT

endmodule


// 32 pipes together, 4 stages fused
//
module DigestQuad #(
        parameter WORDBITS  = 32,
        parameter HASHWORDS = 8,      // hash width in words
        localparam HASHBITS = HASHWORDS * WORDBITS
    ) (
        output wire [HASHBITS - 1 : 0]  a_h_out,
        input                           clk,
        input  wire [HASHBITS - 1 : 0]  a_h_in,
        input  wire [WORDBITS-1:0]      KW0_in,
        input  wire [WORDBITS-1:0]      KW1_in,
        input  wire [WORDBITS-1:0]      KW2_in,
        input  wire [WORDBITS-1:0]      KW3_in
    );
    
    wire [WORDBITS-1:0] abcIn [HASHWORDS - 1 : 0];
    wire [WORDBITS-1:0] kw0In, kw1In, kw2In, kw3In;

    // by convention, all stages synch at input, not at output
    // capture the input at rising clock
    
    generate
      genvar LI;
      for (LI = 0; LI < HASHWORDS; LI = LI + 1)
      begin
        localparam LIW = LI * WORDBITS;
        PassLatch #(.WIDTH(WORDBITS)) ahlatch (.q(abcIn[LI]), .clk(clk), .d(a_h_in[LIW + WORDBITS - 1 : LIW]));
      end
    endgenerate

    PassLatch #(.WIDTH(WORDBITS)) kw0latch (.q(kw0In), .clk(clk), .d(KW0_in));
    PassLatch #(.WIDTH(WORDBITS)) kw1latch (.q(kw1In), .clk(clk), .d(KW1_in));
    PassLatch #(.WIDTH(WORDBITS)) kw2latch (.q(kw2In), .clk(clk), .d(KW2_in));
    PassLatch #(.WIDTH(WORDBITS)) kw3latch (.q(kw3In), .clk(clk), .d(KW3_in));

    // the working bits are 8 x 32-bit words named a thru h
    
    localparam _a = 0;
    localparam _b = 1;
    localparam _c = 2;
    localparam _d = 3;
    localparam _e = 4;
    localparam _f = 5;
    localparam _g = 6;
    localparam _h = 7;

    // the stages generate intermediate versions of a and e
    
    wire [WORDBITS-1:0]    aa, aaa, aaaa, aOut;
    wire [WORDBITS-1:0]    ee, eee, eeee, eOut;

    // These pipes mix the output a and e bits for all 32 words.
    //   The other bits come from shifted pass through. 

    MixAll allMix0 (.a_out(aa), .e_out(ee),
        .a_in(abcIn[_a]), .b_in(abcIn[_b]), .c_in(abcIn[_c]), .d_in(abcIn[_d]),
        .e_in(abcIn[_e]), .f_in(abcIn[_f]), .g_in(abcIn[_g]), .h_in(abcIn[_h]),
        .KW_in(kw0In)
    );

    MixAll allMix1 (.a_out(aaa), .e_out(eee),
        .a_in(aa), .b_in(abcIn[_a]), .c_in(abcIn[_b]), .d_in(abcIn[_c]),
        .e_in(ee), .f_in(abcIn[_e]), .g_in(abcIn[_f]), .h_in(abcIn[_g]),
        .KW_in(kw1In)
    );

    MixAll allMix2 (.a_out(aaaa), .e_out(eeee),
        .a_in(aaa), .b_in(aa), .c_in(abcIn[_a]), .d_in(abcIn[_b]),
        .e_in(eee), .f_in(ee), .g_in(abcIn[_e]), .h_in(abcIn[_f]),
        .KW_in(kw2In)
    );

    MixAll allMix3 (.a_out(aOut), .e_out(eOut),
        .a_in(aaaa), .b_in(aaa), .c_in(aa), .d_in(abcIn[_a]),
        .e_in(eeee), .f_in(eee), .g_in(ee), .h_in(abcIn[_e]),
        .KW_in(kw3In)
    );

    // The a and e bits come from the sum of the mixed carry and sums.
    // Full adders roll up the a and e carry and sum bits to complete their output

    // The other bits come from shifted pass through. 

    assign a_h_out[(_a * WORDBITS) + WORDBITS - 1 : _a * WORDBITS] = aOut;
    assign a_h_out[(_b * WORDBITS) + WORDBITS - 1 : _b * WORDBITS] = aaaa;
    assign a_h_out[(_c * WORDBITS) + WORDBITS - 1 : _c * WORDBITS] = aaa;
    assign a_h_out[(_d * WORDBITS) + WORDBITS - 1 : _d * WORDBITS] = aa;
    assign a_h_out[(_e * WORDBITS) + WORDBITS - 1 : _e * WORDBITS] = eOut;
    assign a_h_out[(_f * WORDBITS) + WORDBITS - 1 : _f * WORDBITS] = eeee;
    assign a_h_out[(_g * WORDBITS) + WORDBITS - 1 : _g * WORDBITS] = eee;
    assign a_h_out[(_h * WORDBITS) + WORDBITS - 1 : _h * WORDBITS] = ee;

endmodule

