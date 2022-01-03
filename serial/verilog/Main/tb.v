`include "main.v"
`timescale 1ns/1ps

module test;
    
    /* Make a reset that pulses once. */
    reg reset = 0;
    initial begin
        # 18 reset = 1;
        # 11 reset = 0;
        # 29 reset = 1;
        # 11 reset = 0;
        # 10000000;
        $finish;
    end

    reg uart_in = 1;
    initial begin
        # 87060 uart_in = 0;
        # 17400 uart_in = 1;
        # 17400 uart_in = 0;
        # 17400 uart_in = 1;
    end

    /* Make a regular pulsing clock. */
    reg clk = 0;
    always # 20 clk = !clk;

    wire [7:0] value;
    wire uart_out;
    wire [7:0] leds;
    wire nouen;
    top t1 (clk, reset, uart_in, uart_out, leds, nouen);

    initial begin
        $dumpfile("test.vcd");
        $dumpvars(0,test);
    end
endmodule