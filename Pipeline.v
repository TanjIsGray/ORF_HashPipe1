//////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023, Avant-Gray LLC.  All rights reserved.
//
// Company: Avant-Gray LLC
// Engineer: Tanj Bennett
// 
// Create Date: 02/07/2022 01:23:20 PM
// Design Name: HashPipe
// File Name: PipeLine.sv
// Project Name: https://github.com/TanjIsGray/LowE_BTC
// 
//  The base algorithm is described in FIPS 180-4:
//  https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
//
//////////////////////////////////////////////////////////////////////////////////

`include "timescale.v"

// Pre-assign word arrays at front of pipeline
//
module PrepareDatasets #(
      parameter WORDBITS  = 32,
      parameter HASHWORDS = 8,        // hash width in words
      parameter MSGWORDS  = 16,       // message width in words
      parameter HASHBITS  = (HASHWORDS * WORDBITS),
      parameter MSGBITS   = (MSGWORDS * WORDBITS)
    ) (
        output wire [MSGBITS-1:0]   W_out,
        output wire [HASHBITS-1:0]  a_h_out,
        input  wire [MSGBITS-1:0]   msg_in
    );

    assign W_out = msg_in;

    localparam [(HASHBITS - 1) : 0] Hinit = {
        //  H[7]                                  ..                                                           H[0]
        32'h5be0cd19, 32'h1f83d9ab, 32'h9b05688c, 32'h510e527f, 32'ha54ff53a, 32'h3c6ef372, 32'hbb67ae85, 32'h6a09e667
    };

    generate
      genvar I;
      for (I = 0; I < HASHWORDS; I = I + 1)
      begin
        localparam IW = I * WORDBITS;
 
        assign a_h_out[IW + WORDBITS - 1 : IW] = Hinit[IW + WORDBITS - 1 : IW];
      end
    endgenerate

 endmodule

// Finish the output hash array at end of pipeline
//
module FinalizeHashWords #(
        parameter WORDBITS  = 32,
        parameter HASHWORDS = 8,       // hash width in words
        parameter HASHBITS  = HASHWORDS * WORDBITS
    ) (
        output wire [HASHBITS-1:0]  a_h_out,
        input  wire [HASHBITS-1:0]  a_h_in
    );

    localparam [(HASHBITS - 1) : 0] Hinit = {
        //  H[7]                                  ..                                                           H[0]
        32'h5be0cd19, 32'h1f83d9ab, 32'h9b05688c, 32'h510e527f, 32'ha54ff53a, 32'h3c6ef372, 32'hbb67ae85, 32'h6a09e667
    };

    generate
      genvar I;
      for (I = 0; I < HASHWORDS; I = I + 1)
      begin
        localparam IW = I * WORDBITS;

        assign a_h_out[IW + WORDBITS - 1 : IW] = Hinit[IW + WORDBITS - 1 : IW] + a_h_in[IW + WORDBITS - 1 : IW];
      end
    endgenerate

endmodule

// 16 quad-stages in a pipeline

module WholeQuadPipe #(
        parameter WORDBITS  = 32,
        parameter HASHWORDS = 8,       // hash width in words
        parameter MSGWORDS  = 16,       // message width in words
        parameter HASHBITS  = HASHWORDS * WORDBITS,
        parameter MSGBITS   = MSGWORDS * WORDBITS
    ) (
        output wire [HASHBITS-1:0]  a_h_out,
        input                       clk,
        input  wire [MSGBITS-1:0]   msg_in
    );

    wire [MSGBITS-1:0]    W[12:0];    // [17];
    wire [HASHBITS-1:0]   a_h[16:0];  // [17];

    PrepareDatasets prep (.W_out(W[0]), .a_h_out(a_h[0]), .msg_in(msg_in));

    // Kube-root constants: FIPS 180-4, §4.2.2
    // packing this way, the left word is K[0] at bit position (63 * 32)
   localparam [(WORDBITS * 64) - 1 : 0] Kconst = {
//          [00]
       32'h428a2f98, 32'h71374491, 32'hb5c0fbcf, 32'he9b5dba5,
       32'h3956c25b, 32'h59f111f1, 32'h923f82a4, 32'hab1c5ed5,
       32'hd807aa98, 32'h12835b01, 32'h243185be, 32'h550c7dc3,
       32'h72be5d74, 32'h80deb1fe, 32'h9bdc06a7, 32'hc19bf174,
       32'he49b69c1, 32'hefbe4786, 32'h0fc19dc6, 32'h240ca1cc,
       32'h2de92c6f, 32'h4a7484aa, 32'h5cb0a9dc, 32'h76f988da,
       32'h983e5152, 32'ha831c66d, 32'hb00327c8, 32'hbf597fc7,
       32'hc6e00bf3, 32'hd5a79147, 32'h06ca6351, 32'h14292967,
       32'h27b70a85, 32'h2e1b2138, 32'h4d2c6dfc, 32'h53380d13,
       32'h650a7354, 32'h766a0abb, 32'h81c2c92e, 32'h92722c85,
       32'ha2bfe8a1, 32'ha81a664b, 32'hc24b8b70, 32'hc76c51a3,
       32'hd192e819, 32'hd6990624, 32'hf40e3585, 32'h106aa070,
       32'h19a4c116, 32'h1e376c08, 32'h2748774c, 32'h34b0bcb5,
       32'h391c0cb3, 32'h4ed8aa4a, 32'h5b9cca4f, 32'h682e6ff3,
       32'h748f82ee, 32'h78a5636f, 32'h84c87814, 32'h8cc70208,
       32'h90befffa, 32'ha4506ceb, 32'hbef9a3f7, 32'hc67178f2
//                                                    [63]
   };

       wire [WORDBITS-1:0]  KW [63:0];

// quad 0
        localparam SW0 = (4 * (15 - 0)) * WORDBITS;    // reversed word order

        ScheduleMessageQuad #(
                .K0_constIn(Kconst[SW0 + ((4 * WORDBITS) - 1) : SW0 + (3 * WORDBITS)]),
                .K1_constIn(Kconst[SW0 + ((3 * WORDBITS) - 1) : SW0 + (2 * WORDBITS)]),
                .K2_constIn(Kconst[SW0 + ((2 * WORDBITS) - 1) : SW0 + WORDBITS]),
                .K3_constIn(Kconst[SW0 + (WORDBITS - 1) : SW0])
          ) schedQ_0 (
            .KW0_out(KW[0]), .KW1_out(KW[1]), .KW2_out(KW[2]), .KW3_out(KW[3]), .W_out(W[1]), .clk(clk), .W_in(W[0])
        );

        DigestQuad digQ_0 (
            .a_h_out(a_h[1]), .clk(clk), .a_h_in(a_h[0]), .KW0_in(KW[0]), .KW1_in(KW[1]), .KW2_in(KW[2]), .KW3_in(KW[3])
        );

// while exploring OpenRoad we can make the pipeline shorter so builds are quicker.  Set STOPSTAGE to 12 for production.

    localparam STOPSTAGE = 2;

    generate
        genvar STAGE;
        for (STAGE = 1; STAGE < STOPSTAGE; STAGE = STAGE + 1)  //***64
        begin

            localparam SW = (4 * (15 - STAGE)) * WORDBITS;    // reversed word order
            localparam QW = 4 * STAGE;

            ScheduleMessageQuad #(
                .K0_constIn(Kconst[(SW + (4 * WORDBITS) - 1) : SW + (3 * WORDBITS)]),
                .K1_constIn(Kconst[(SW + (3 * WORDBITS) - 1) : SW + (2 * WORDBITS)]),
                .K2_constIn(Kconst[(SW + (2 * WORDBITS) - 1) : SW + WORDBITS]),
                .K3_constIn(Kconst[(SW + WORDBITS - 1) : SW])
            ) schedQ (
                .KW0_out(KW[QW]), .KW1_out(KW[QW+1]), .KW2_out(KW[QW+2]), .KW3_out(KW[QW+3]),
                .W_out(W[STAGE + 1]),
                .clk(clk),
                .W_in(W[STAGE])
            );

            DigestQuad digQ (
                .a_h_out(a_h[STAGE + 1]),
                .clk(clk),
                .a_h_in(a_h[STAGE]),
                .KW0_in(KW[QW]), .KW1_in(KW[QW+1]), .KW2_in(KW[QW+2]), .KW3_in(KW[QW+3])
            );
        end
    endgenerate

// The remaining 4 stages have shrinking Schedule requirements.
 
// quad 12
        wire [(12 * WORDBITS)-1:0]    W13;

        localparam SW12 = (4 * (15 - 12)) * WORDBITS;    // reversed word order
    
        ScheduleMessageFinalQuads #(.WINWORDS(16),
                .K0_constIn(Kconst[(SW12 + (4 * WORDBITS) - 1) : SW12 + (3 * WORDBITS)]),
                .K1_constIn(Kconst[(SW12 + (3 * WORDBITS) - 1) : SW12 + (2 * WORDBITS)]),
                .K2_constIn(Kconst[(SW12 + (2 * WORDBITS) - 1) : SW12 + WORDBITS]),
                .K3_constIn(Kconst[(SW12 + WORDBITS - 1) : SW12])
          ) schedQ_12 (
            .KW0_out(KW[48]), .KW1_out(KW[49]), .KW2_out(KW[50]), .KW3_out(KW[51]), .W_out(W13), .clk(clk), .W_in(W[STOPSTAGE])
        );

        DigestQuad digQ_12 (
            .a_h_out(a_h[13]), .clk(clk), .a_h_in(a_h[STOPSTAGE]), .KW0_in(KW[48]), .KW1_in(KW[49]), .KW2_in(KW[50]), .KW3_in(KW[51])
        );

// quad 13
        wire [(8 * WORDBITS)-1:0]    W14;

        localparam SW13 = (4 * (15 - 13)) * WORDBITS;    // reversed word order
    
        ScheduleMessageFinalQuads #(.WINWORDS(12),
                .K0_constIn(Kconst[(SW13 + (4 * WORDBITS) - 1) : SW13 + (3 * WORDBITS)]),
                .K1_constIn(Kconst[(SW13 + (3 * WORDBITS) - 1) : SW13 + (2 * WORDBITS)]),
                .K2_constIn(Kconst[(SW13 + (2 * WORDBITS) - 1) : SW13 + WORDBITS]),
                .K3_constIn(Kconst[(SW13 + WORDBITS - 1) : SW13])
          ) schedQ_13 (
            .KW0_out(KW[52]), .KW1_out(KW[53]), .KW2_out(KW[54]), .KW3_out(KW[55]), .W_out(W14), .clk(clk), .W_in(W13)
        );

        DigestQuad digQ_13 (
            .a_h_out(a_h[14]), .clk(clk), .a_h_in(a_h[13]), .KW0_in(KW[52]), .KW1_in(KW[53]), .KW2_in(KW[54]), .KW3_in(KW[55])
        );

// quad 14
        wire [(4 * WORDBITS)-1:0]    W15;

        localparam SW14 = (4 * (15 - 14)) * WORDBITS;    // reversed word order
    
        ScheduleMessageFinalQuads #(.WINWORDS(8),
                .K0_constIn(Kconst[(SW14 + (4 * WORDBITS) - 1) : SW14 + (3 * WORDBITS)]),
                .K1_constIn(Kconst[(SW14 + (3 * WORDBITS) - 1) : SW14 + (2 * WORDBITS)]),
                .K2_constIn(Kconst[(SW14 + (2 * WORDBITS) - 1) : SW14 + WORDBITS]),
                .K3_constIn(Kconst[(SW14 + WORDBITS - 1) : SW14])
          ) schedQ_14 (
            .KW0_out(KW[56]), .KW1_out(KW[57]), .KW2_out(KW[58]), .KW3_out(KW[59]), .W_out(W15), .clk(clk), .W_in(W14)
        );

        DigestQuad digQ_14 (
            .a_h_out(a_h[15]), .clk(clk), .a_h_in(a_h[14]), .KW0_in(KW[56]), .KW1_in(KW[57]), .KW2_in(KW[58]), .KW3_in(KW[59])
        );

// quad 15
        localparam SW15 = (4 * (15 - 15)) * WORDBITS;    // reversed word order
    
        ScheduleMessageQuad15 #(
                .K0_constIn(Kconst[(SW15 + (4 * WORDBITS) - 1) : SW15 + (3 * WORDBITS)]),
                .K1_constIn(Kconst[(SW15 + (3 * WORDBITS) - 1) : SW15 + (2 * WORDBITS)]),
                .K2_constIn(Kconst[(SW15 + (2 * WORDBITS) - 1) : SW15 + WORDBITS]),
                .K3_constIn(Kconst[(SW15 + WORDBITS - 1) : SW15])
          ) schedQ_15 (
            .KW0_out(KW[60]), .KW1_out(KW[61]), .KW2_out(KW[62]), .KW3_out(KW[63]), .clk(clk), .W_in(W15)
        );

        DigestQuad digQ_15 (
            .a_h_out(a_h[16]), .clk(clk), .a_h_in(a_h[15]), .KW0_in(KW[60]), .KW1_in(KW[61]), .KW2_in(KW[62]), .KW3_in(KW[63])
        );

    FinalizeHashWords finalQ (.a_h_out(a_h_out), .a_h_in(a_h[16]));

endmodule

