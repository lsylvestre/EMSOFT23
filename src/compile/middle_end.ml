open Ast

open Display_internal_steps

let compile (pi:pi) : pi =
  (** renaming all names in the source program *)
  let pi = Ast_rename.rename_pi pi in
  display_pi Rename pi;

  (** ensure that each function has a name *)
  let pi = Name_anonymus_functions.name_pi pi in

  (** put the program in ANF-form *)
  let pi = Anf.anf_pi pi in
  display_pi Anf pi;

  assert (Anf.in_anf_pi pi);

  (** move-up let-bindings (optional) *)
  let pi = Let_floating.let_floating_pi pi in
  display_pi Let_floating pi;

  assert (Anf.in_anf_pi pi);

  (** eliminating higher-order functions *)
  let pi = Specialize.specialize_pi pi in
  let pi = Ast_rename.rename_pi pi in
  display_pi Specialize pi;

  let pi = Anf.anf_pi pi in
  let pi = Lambda_lifting.lambda_lifting_pi pi in
  display_pi Lambda_lifting pi;

  (* let _ = Typing.typing_with_argument pi [_] in *)

  (* replicates toplevel definition within certain constructs *)
  let pi = Move_down_gfun_under_register.move_down_gfun_under_register_pi pi in

  let pi = Ast_rename.rename_pi pi in
  let pi = Ast_ensure_rec_naming.ensure_rec_naming_pi pi in 
  
  (** inlining of instantaneous functions *)
  
  let pi = Inline.inl_pi pi in
  let pi = Ast_rename.rename_pi pi in
  display_pi Inline pi;
  
  let pi = Deadcode_elimination.deadcode_elimination_pi pi in

  let pi = Anf.anf_pi pi in

  let pi = Matching.matching_pi pi in
  display_pi Matching pi;
  
  let pi = Propagate_global_constant.propagate_global_constant_pi pi in

  let pi = Anf.anf_pi pi in
  let pi = Propagation.propagation_pi pi in

  (** filtering [pi.ds] to keep-only functions needed by [pi.main] (and their dependancies).
      Current compilation scheme to FSM (see [fsm_comp.ml]) produces ill-typed code
      if this transformation is not performed *)

  (* put a fresh name to each register *)
  let pi = Instantiate.instantiate_pi pi in

  pi
