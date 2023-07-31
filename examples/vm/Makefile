NS=2000

FLAGS=
SRC=tests/fun4_env.ml

byte:
	(cd bc ; make SRC=../$(SRC))

vm:	byte
	(cd ../..; ./mixc $(FLAGS) \
			examples/vm/ram.ml \
	        	examples/vm/runtime.ml \
			examples/vm/bytecode.ml \
			examples/vm/vm.ml -relax)


simul:	vm
	(cd ../../vhdl; make NS=$(NS))

clean:
	(cd bc ; make -f Makefile clean)
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	rm -f $(EXE)

