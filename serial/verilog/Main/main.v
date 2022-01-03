`include "serial/serial.v"
`include "txuartlite.v"
`include "rxuartlite.v"

`timescale 100fs/100fs
module top
    ( // Inputs
      input  clk_25mhz // clock
    , input  reset // reset
    , input ftdi_txd
    , output ftdi_rxd

      // Outputs
    , output wire [7:0] led
    , output wifi_gpio0
    );
    wire o_uart_tx;
    wire i_uart_rx;
    assign ftdi_rxd = o_uart_tx;
    assign i_uart_rx = ftdi_txd;

    reg [7:0] o_led;
    assign led = o_led;

    assign wifi_gpio0 = 1'b1;

    wire enable;
    assign enable = 1'b1;

    parameter CLOCK_RATE_HZ = 25_000_000;    // System clock rate in Hz
    parameter BAUD_RATE     =    115_200;    // Baud rate
    parameter CLOCKS_PER_BAUD = CLOCK_RATE_HZ/BAUD_RATE;

    // Interface to the UART
    reg  [7:0] tx_data;           // Data to send to the UART
    wire       tx_busy;           // Is it busy?
    reg        tx_stb;            // Strobe to ask to send data
    initial    tx_stb= 0;
    wire [7:0] rx_data;           // Each char typed by user
    wire rx_avail;                // If true, user data is available


    txuartlite #(CLOCKS_PER_BAUD[23:0])
        transmitter(clk_25mhz, tx_stb, tx_data, o_uart_tx, tx_busy);
    rxuartlite #(CLOCKS_PER_BAUD[23:0])
        receiver(clk_25mhz, i_uart_rx, rx_avail, rx_data);

    wire can_send;
    assign can_send = !tx_stb && !tx_busy;

    always @(posedge clk_25mhz) begin
        o_led <= rx_data;
        tx_stb  <= tx_and_stb[0];
        tx_data <= tx_and_stb[8:1];
    end

    wire [8:0] tx_and_stb;

    serial s0 (
        .clk (clk_25mhz), 
        .reset (reset), 
        .enable (enable), 
        .cansend (can_send),
        .out (tx_and_stb) );

endmodule