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
	clash --verilog Serial.hs -outputdir .hs -fclash-hdldir verilog/

all: clash json config bitstream
noclash: json config bitstream

clean:
	rm -r *.hi *.o *.dyn_hi *.dyn_o project.json test.vcd sim.vvp ulx3s_out.config ulx3s.bit verilog/Serial.topEntity/

prog:
	fujprog ulx3s.bit

sim:
	-rm sim.vvp test.vcd
	iverilog verilog/tb.v -I verilog -o sim.vvp
	vvp sim.vvp