SRC= ../tests/v42.ml

all:	tmp
	ocamlc -I ../ ../customStdlib.ml  $(SRC) -without-runtime -no-alias-deps   -no-app-funct \
	-no-check-prims   -noassert    -noautolink    -nolabels -nopervasives
	ocamlclean a.out
	dune exec bcprepare a.out

tmp:
	mkdir tmp

clean:
	rm -rf a.out
	rm -rf tmp/code.txt
	dune clean
