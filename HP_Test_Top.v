//////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023, Avant-Gray LLC.  All rights reserved.
//
// Company: Avant-Gray LLC
// Engineer: Tanj Bennett
// 
// Create Date: 10/03/2022
// Design Name: HashPipe
// File Name: HP_Test_Top
// Project Name: https://github.com/TanjIsGray/LowE_BTC
// 
//////////////////////////////////////////////////////////////////////////////////

`include "timescale.v"

//  SHA-256 hash function variants for unrolling in special case of BTC mining.

//  The base algorithm is described in FIPS 180-4:
//  https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf

/// Test driver
///
module HP_Test_Top #(
  parameter
        WORDBITS  = 32,
        HASHWORDS = 8,       // hash width in words
        MSGWORDS  = 16,      // message width in words
        parameter HASHBITS  = HASHWORDS * WORDBITS,
        parameter MSGBITS   = MSGWORDS * WORDBITS
) (
    input                       inPort_clk,
    input  [MSGBITS-1:0]        inPort_msg,
    output reg [HASHBITS-1:0]   outPort_hash
);

    reg	[MSGBITS-1:0]	        req_msg_r;

    always @(posedge inPort_clk) begin
        req_msg_r <= inPort_msg;
    end

    wire [HASHBITS-1:0]  quadOutput;

    WholeQuadPipe quadPipe (
        .a_h_out(quadOutput),
        .clk(inPort_clk),
        .msg_in(req_msg_r)
    );

    always @(posedge inPort_clk) begin
        outPort_hash <= quadOutput;
    end

endmodule
