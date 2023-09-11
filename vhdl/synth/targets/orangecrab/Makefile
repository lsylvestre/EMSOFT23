YOSYS_HQ=~/oss-cad-suite


YOSYS_FLAGS= -abc2 -retime

init:
	source $(YOSYS_HQ)/environment


front:
	ghdl -a ../../../runtime.vhdl ../../../main.vhdl ../../top.vhdl

synth:	front
	yosys -g -m ghdl.so -p "ghdl --latches top; synth_ecp5 -top top $(YOSYS_FLAGS) -json top.json"

# place and route
pnr:	synth
	nextpnr-ecp5 --json top.json --textcfg top_out.config --85k --package CSFBGA285 --lpf orangecrab_r0.2.pcf --log log.txt


clean:
	rm -rf log.* work-obj93.cf top.json top_out.config
