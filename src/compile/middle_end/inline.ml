open Ast
open Pattern
open Ast_subst

(* inline a program given in ANF, lambda-lifted form. The resulting program is
   in lambda-lifted-form but not necessarily in ANF-form. *)

(** [simplify e] gives a non-ANF expression in which non-recursive functions
    are inlined in functional position at each call site
    (e.g. [((fun x -> x), 42)]) *)
let rec simplify e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ ->
      (* no subexpressions *)
      e
  | E_if(e1,e2,e3) ->
      assert(Anf.is_xc e1);
      (* e1 is of type bool and is initially a variable x (if in ANF-form),
         so, x cannot be substituted by a function) *)
      E_if(e1,simplify e2,simplify e3)
  | E_letIn(p,e1,e2) ->
      E_letIn(p,simplify e1,simplify e2)
  | E_fun(x,e) ->
      E_fun(x,simplify e)
  | E_fix(f,(p,e)) ->
      E_fix(f,(p,simplify e))
  | E_app(E_fun(p,e1),e2) ->
     (* substitution is needed (rather than a let-binding)
        since e2 could be a function (fun x -> e3) *)
     simplify @@ subst_p_e p e2 e1
  | E_app(e1,e2) ->
      E_app(simplify e1,simplify e2)
  | E_tuple(es) ->
      E_tuple (List.map simplify es)
  | E_lastIn(x,e1,e2) ->
      let y = gensym () in
      E_lastIn(y,simplify e1,subst_e x (E_var y) (simplify e2))
  | E_set(x,e1) ->
      assert(Anf.is_xc e1);
      e
  | E_static_array_get(x,e1) ->
      assert(Anf.is_xc e1);
      e
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      assert(Anf.is_xc e1);
      assert(Anf.is_xc e2);
      e
  | E_step _ | E_par _ -> e (* do not transform sub-expressions under step and // *)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)


(** [inl pi] inlines non-recursive functions in program [pi].
   assumes that [ds] bind name to functions ([fun x -> e] or [fix f (fun x -> e)]) *)
let rec inl (ds,e) =
  let rec aux recd_and_globals ds e =
    match ds with
    | [] ->
       List.rev_map (fun (x,e) -> x,simplify e) recd_and_globals, simplify e
    | (x,(E_fix(f,(p,ef))))::fs' ->
        assert (not (pat_mem f p));
        aux ((x,E_fix(x,(p,subst_e f (E_var x) ef)))::recd_and_globals) fs' e
    | (x,(E_fun _ as ex))::ds' -> (* super-static environment *)
       let ss e = (subst_e x ex e) in
       aux recd_and_globals (List.map (fun (x,e) -> x, ss e) ds') (ss e)
    | (x,e2)::fs' -> aux ((x,e2)::recd_and_globals) fs' e
       (* assume that [e] is the global definition of a constant or a static array *)
  in
  aux [] ds e

let inl_pi pi =
  let ds,main = inl (pi.ds, pi.main) in
  { pi with ds ; main }
