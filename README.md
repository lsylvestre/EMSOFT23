# Work-in-Progress: mixing computation and interaction on FPGA
-------

This tool implements a type-checker (*typing/typing.ml*), an interpreter (*interp/interp.ml*) 
and a compiler (*compile/middle_end*) targeting VHDL (*compile/back_end/target/gen_vhdl.ml*), 
for a simple functional-imperative language (*syntax/ast.ml*).

The language is used to program reactive embedded applications on FPGA as synchronous programs 
mixing *computations* (e.g. algorithms on data structures) 
and *interaction*  (i.e., reading inputs and producing outputs in a reactive way).

Examples are provided in folder `examples`.

Installation
------------

To build from source, the pre-requisites are:

* `ocaml` (>= 4.11.1) with the following package installed
  - `menhir (version 20220210)`

From the root of the source tree:

1. `make`
