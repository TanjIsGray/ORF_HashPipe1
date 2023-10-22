`timescale 1ns / 10ps
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
    input                       clk,
    input  [MSGBITS-1:0]        req_msg,
    output reg [HASHBITS-1:0]   resp_hash
);

    reg	[MSGBITS-1:0]	        req_msg_r;

    PassLatch #(.WIDTH(MSGBITS)) msgIn (.q(req_msg_r), .clk(clk), .d(req_msg));

    wire [HASHBITS-1:0]  quadOutput;
  
    WholeQuadPipe quadPipe (
        .a_h_out(quadOutput),
        .clk(clk),
        .msg_in(req_msg)
    );

    PassLatch #(.WIDTH(HASHBITS)) hashOut (.q(resp_hash), .clk(clk), .d(quadOutput));

endmodule
