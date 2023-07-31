CAMLC=ocamlc
CAMLLEX=ocamllex
CAMLDEP=ocamldep

EXE=mixc

FLAGS=

INCLUDES=-I src -I src/syntax \
         -I src/frontend \
         -I src/typing \
         -I src/interp \
         -I src/compile \
         -I src/compile/middle_end \
         -I src/compile/inspect \
         -I src/compile/backend \
         -I src/target \
         -I src/compile/backend/gluecode \
         -I src/compile/test

MENHIR=menhir --unused-tokens --unused-precedence-levels --infer --ocamlc "$(CAMLC) -i $(INCLUDES)"

OBJS= src/prelude.cmo \
      src/typing/types.cmo \
      src/operators.cmo \
      src/syntax/ast.cmo \
      src/syntax/pattern.cmo \
      src/syntax/ast_pprint.cmo \
      src/syntax/ast_subst.cmo \
      src/syntax/ast_rename.cmo \
      src/syntax/free_vars.cmo \
      src/syntax/map_pi.cmo \
      src/syntax/ast_ensure_rec_naming.cmo \
      src/syntax/ast_undecorated.cmo \
      src/frontend/fix_int_lit_size.cmo \
      src/typing/typing.cmo \
      src/syntax/ast_mk.cmo \
      src/compile/combinatorial.cmo \
      src/compile/encode_reg_exec.cmo \
      src/compile/middle_end/name_anonymus_functions.cmo \
      src/compile/middle_end/anf.cmo \
      src/compile/middle_end/let_floating.cmo \
      src/compile/middle_end/lambda_lifting.cmo \
      src/compile/middle_end/inline.cmo \
      src/compile/middle_end/propagation.cmo \
      src/compile/middle_end/instantiate.cmo \
      src/compile/middle_end/specialize.cmo \
      src/compile/middle_end/fun_shape_entry_point.cmo \
      src/compile/middle_end/rename_main_arg.cmo \
      src/compile/middle_end/move_down_gfun_under_register.cmo \
      src/compile/middle_end/deadcode_elimination.cmo \
      src/compile/middle_end/matching.cmo \
      src/target/naming_convention.cmo \
      src/compile/middle_end/sharing.cmo \
      src/compile/inspect/display_internal_steps.cmo \
      src/compile/middle_end.cmo \
      src/target/fsm_syntax.cmo \
      src/compile/inspect/display_target.cmo \
      src/target/fsm_typing.cmo \
      src/compile/backend/fsm_comp.cmo \
      src/compile/backend/flat_transitions.cmo \
      src/compile/backend/flat_let_atom.cmo \
      src/compile/backend/list_machines.cmo \
      src/target/gen_vhdl.cmo \
      src/compile/compile.cmo \
      src/compile/test/gen_testbench.cmo\
      src/interp/norm.cmo \
      src/interp/interp.cmo \
      src/frontend/current_filename.cmo \
      src/frontend/parser.cmo \
      src/frontend/lexer.cmo \
      src/frontend/frontend.cmo \
      src/frontend/clean_simul.ml \
      src/compile/backend/gluecode/gen_bsp_update_tcl.ml \
      src/compile/backend/gluecode/gen_hw_tcl.ml \
      src/compile/backend/gluecode/gen_platform_tcl.ml \
      src/compile/backend/gluecode/gen_glue_code.ml \
      src/main.cmo

SRCS= `find src -name "*.ml*"`

all:	depend src/frontend/parser.cmi $(OBJS)
	$(CAMLC) $(FLAGS) $(INCLUDES) -o $(EXE) $(OBJS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi

.ml.cmo:
	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDES) $(FLAGS) -c $<

.mly.ml:
	$(MENHIR) $<

.mll.ml:
	$(CAMLLEX) $<

depend:
	$(CAMLDEP) $(INCLUDES) $(SRCS) > src/.depend
	menhir --depend src/frontend/parser.mly >> src/.depend

include src/.depend


prepare:  vhdl
	mkdir -p vhdl/bsp
	mkdir -p vhdl/qsys

NS=2000

simul:
	cd vhdl; make NS=$(NS)

clean:
	rm -f `find . -name "*.cmo"`
	rm -f `find . -name "*.cmi"`
	rm -f $(EXE)

clean-all:	clean
	make -f vhdl/Makefile clean-all
