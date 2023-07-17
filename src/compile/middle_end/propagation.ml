open Ast
open Ast_subst

let rec constant_or_copy = function
| E_deco (e,_) ->
    Ast_undecorated.still_decorated e
| E_const _ | E_var _ ->
    true
| E_tuple es ->
    List.for_all constant_or_copy es
| _ -> false


let nb_modif = ref 0 ;;

let rec prop e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ ->
      e
  | E_if(e1,e2,e3) ->
      E_if(prop e1,prop e2,prop e3)
  | E_letIn(p,((E_tuple es) as e),e2) when List.for_all constant_or_copy es ->
      incr nb_modif;
      prop (subst_p_e p e e2)
  | E_letIn(p,e1,e2) when evaluated e1 ->
      incr nb_modif;
      prop (subst_p_e p e1 e2)
  | E_letIn(p,e1,e2) ->
      if constant_or_copy e1 then
       (incr nb_modif; prop (subst_p_e p e1 e2)) else
      E_letIn(p,prop e1,prop e2)
  | E_fun(x,e) ->
      E_fun(x,prop e)
  | E_fix(f,(x,e)) ->
      E_fix(f,(x,prop e))
  | E_app(E_const(Op(GetTuple{pos;arity})),E_tuple vs) ->
      incr nb_modif;
      prop @@ List.nth vs pos
  | E_app(e1,e2) ->
      E_app(prop e1,prop e2)
  | E_tuple aas ->
      E_tuple aas
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,prop e1,prop e2)
  | E_set(x,e1) ->
      E_set(x,prop e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,prop e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,prop e1,prop e2)
  | E_step _ | E_par _ -> e (* do not transform sub-expressions under step and // *)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)

let rec prop_n e =
  nb_modif := 0;
  let e' = prop e in
  if !nb_modif > 0 then prop_n e' else e'

let propagation (ds,e) =
  List.map (fun (x,e) -> x,prop_n e) ds, prop_n e

let propagation_pi pi =
  Map_pi.map prop_n pi
