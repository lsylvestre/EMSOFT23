NS=2000

FLAGS=
SRC=tests/v42.ml

all: byte vm
	echo "done"

byte:
	(cd bc ; make SRC=../$(SRC))

VM=examples/vm

vm:
	(cd ../..; ./mixc $(FLAGS) \
	        $(VM)/mlvalue.ml \
	        $(VM)/ram.ml \
	        $(VM)/runtime.ml \
	        $(VM)/alloc.ml \
	        $(VM)/prims.ml \
			$(VM)/bytecode.ml \
			$(VM)/debug.ml \
			$(VM)/vm.ml -relax -clk-top=clk48 -top "usr_btn : 1 | rgb_led0_r : 1, rgb_led0_b : 1")

simul:
	(cd ../../vhdl; make NS=$(NS))

clean:
	(cd bc ; make -f Makefile clean)
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	rm -f $(EXE)

