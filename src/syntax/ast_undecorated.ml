open Ast

(** [remove_deco e] removes both locations and constructs [(e : t)].
   In other word, [(e : t)] is only taking into account during typing,
   but no during code generation ! Solution : other constructs keep trace
   of such type constraints (e.g. litteral constants for integer, which include their size type)
 *)

exception Still_decorated of e

let still_decorated e =
  raise (Still_decorated e)

let rec remove_deco e =
  match e with
  | E_deco(e,_) ->
      remove_deco e
  | E_var _ | E_const _ -> e
  | E_fun(x,e1) ->
      E_fun(x,remove_deco e1)
  | E_fix(f,(x,e1)) ->
      E_fix(f,(x,remove_deco e1))
  | E_if(e1,e2,e3) ->
      E_if(remove_deco e1,remove_deco e2,remove_deco e3)
  | E_match(e,hs,e_els) ->
      E_match(remove_deco e,List.map (fun (c,e) -> c,remove_deco e) hs,remove_deco e_els)
  | E_letIn(x,e1,e2) ->
      E_letIn(x,remove_deco e1,remove_deco e2)
  | E_tuple(es) ->
      E_tuple(List.map remove_deco es)
  | E_app(E_const(Op(TyConstr _)),e) -> (* remove also type annotations [(e : t)] *)
      remove_deco e
  | E_app(e1,e2) ->
      E_app(remove_deco e1,remove_deco e2)
  | E_reg(V ev,e0) ->
      E_reg(V (remove_deco ev),remove_deco e0)
  | E_exec(e1,e2,l) ->
      E_exec(remove_deco e1,remove_deco e2,l)
  | E_lastIn(x,e1,e2) ->
     E_lastIn(x,remove_deco e1,remove_deco e2)
  | E_set(x,e1) ->
      E_set(x,remove_deco e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,remove_deco e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,remove_deco e1,remove_deco e2)
  | E_step(e1,k) ->
      E_step(remove_deco e1,k)
  | E_par(e1,e2) ->
      E_par(remove_deco e1,remove_deco e2)

let remove_deco_pi pi =
  Map_pi.map remove_deco pi

