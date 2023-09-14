## Synthesis on an OrangeCrab FPGA board using an open source tool chain


Installation
------------

To build from source, the pre-requisites are:

* `oss-cad-suite` (https://github.com/YosysHQ/oss-cad-suite-build)
* `ghdl-yosys-plugin` (https://github.com/ghdl/ghdl-yosys-plugin)

From this directory:

```
$ make front
$ make synth
$ make pnr
$ make pack
$ make dfu

$ make clean
```