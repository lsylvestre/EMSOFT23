## Synthesis on an OrangeCrab FPGA board using an open source tool chain


Installation
------------

To build from source, the pre-requisites are:

* `oss-cad-suite` (https://github.com/YosysHQ/oss-cad-suite-build)
* `ghdl-yosys-plugin` (https://github.com/ghdl/ghdl-yosys-plugin)

From this directory:

```
$ front
$ synth
$ pnr

...

Info: Max frequency for clock '$glbnet$clk48$TRELLIS_IO_IN': 48.36 MHz (PASS at 48.00 MHz)
Info: Program finished normally.

$ clean
```