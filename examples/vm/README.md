## Ongoing implementation of the OCaml Virtual Machine

Files
----
- *customStdlib.ml*, simplified OCaml standard librarie
- *ram.ml*, *runtime.ml*, *vm.ml* : source code of the Virtual Machine (VM) to be compile in VHDL
- *bytecode.ml* generated code as part of the VM implementation



Installation
------------

To build from source, the pre-requisites are:

* `ocaml`
* `opam`
* `ocamlclean`
* `obytelib`
* `dune`

From this directory (*examples/vm*)

1. `make vm PROG=demo/demo.ml`
2. `make simul NS=3000` *(simulating the program for 3000 nanoseconds)*


Demo
----

```
$ make vm SRC=demo/demo.ml 

...

vhdl code generated in vhdl/main.vhdl 
testbench generated in vhdl/tb_main.vhdl for software RTL simulation using GHDL.
  info: circuit  "main"  generated in folder vhdl/.
$
```

```
$ make simul NS=8000
(cd ../../vhdl; make NS=8000)
ghdl -a  runtime.vhdl
ghdl -a  main.vhdl
ghdl -a  tb_main.vhdl
ghdl -e  tb_main
ghdl -r  tb_main --vcd=tb.vcd --stop-time=6500ns
pc:0|acc:0|sp:2000 
pc:36|acc:0|sp:2000 
pc:38|acc:20002|sp:2000 
pc:39|acc:20002|sp:2001 
pc:40|acc:20016|sp:2001 
pc:41|acc:3|sp:2001 
pc:43|acc:1|sp:2001 
pc:47|acc:8006|sp:2002 
pc:48|acc:1|sp:2002 
pc:50|acc:1|sp:2002 
pc:52|acc:31|sp:2003 
pc:54|acc:8012|sp:2002 
pc:55|acc:8012|sp:2003 

pc:18|acc:241|sp:2016 
pc:16|acc:241|sp:2012 
pc:17|acc:13|sp:2013 
pc:18|acc:1441|sp:2012 
pc:107|acc:1441|sp:2008 
======> 720 
pc:109|acc:1|sp:2008 
pc:111|acc:1|sp:2000 
STOP : pc:111|acc:1|sp:2000 

...

STOP : pc:111|acc:1|sp:2000 
ghdl:info: simulation stopped by --stop-time @8us
$ 
```