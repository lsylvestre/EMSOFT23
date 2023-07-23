(* Heuristic to determine if a given expression is combinatorial *)

open Ast

let op_combinatorial (op:op) : bool =
	match op with
	| (Print)
	| (To_string)
	| (Random)
	| (Assert) -> false
	| _ -> true


let const_combinatorial (c:c) : bool =
	match c with
  | External _ -> false
  | _ -> true

let rec combinatorial (e:e) : bool =
  match e with
	| E_deco (e,_) ->
	    combinatorial e
	| E_var _ ->
	    true
	| E_const c -> 
	    const_combinatorial c
	| E_if(e1,e2,e3) ->
	    combinatorial e1 && combinatorial e2 && combinatorial e3
	| E_match(e1,hs,e_els) ->
      (* hard to defined as a combinatorial function being generic in the number of cases *)
      false
	| E_app(E_const(Op op),e2) ->
	    op_combinatorial op && combinatorial e2
	| E_app(e1,e2) ->
	    (* as we can't know locally whether [e1] is combinatorial, 
	       we return false *)
	    false
	| E_letIn(_,e1,e2) ->
	    combinatorial e1 && combinatorial e2
	| E_tuple es ->
	    List.for_all combinatorial es
	| E_fun _ | E_fix _ ->
	    false
	| E_lastIn _ | E_set _ | E_step _ | E_par _ ->
	    false
	| E_reg _ | E_exec _ ->
	    false (* have an internal state *)
	| E_static_array_length _ ->
	    true 
	| E_static_array_get _ ->
	    false
	| E_static_array_set _ ->
	    false (* side effect *)
