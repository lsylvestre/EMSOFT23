## Synthesis on an OrangeCrab FPGA board using an open source tool chain


Installation
------------

To build from source, the pre-requisites are:

* `oss-cad-suite` (https://github.com/YosysHQ/oss-cad-suite-build)
* `ghdl-yosys-plugin` (https://github.com/ghdl/ghdl-yosys-plugin)

From this directory:

```
ghdl -a ../../../runtime.vhdl ../../../main.vhdl ../../top.vhdl 
yosys -g -m ghdl.so -p "ghdl --latches top; synth_ecp5 -json top.json"
```