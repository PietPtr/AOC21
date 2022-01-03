.PHONY: sim

bitstream:
	ecppack ulx3s_out.config ulx3s.bit

nextpnr: 
	nextpnr-ecp5 --85k --json project.json \
		--lpf constraints.lpf \
		--textcfg ulx3s_out.config 

yosys: 
	yosys project.ys 

clash:
	-rm project.json
	clash --verilog Serial.hs

all: clash json config bitstream
noclash: json config bitstream

clean:
	rm *.hi *.o *.dyn_hi *.dyn_o project.json

prog:
	fujprog ulx3s.bit

sim:
	-rm sim test.vcd
	iverilog verilog/Main/tb.v -I verilog/Main/ -o sim.vvp
	vvp sim.vvp