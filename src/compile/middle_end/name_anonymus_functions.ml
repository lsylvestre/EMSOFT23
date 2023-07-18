open Ast
open Ast_subst

let rec name e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> e
  | E_fun(x,e1) -> (* anonymus functions are named *)
      let f = gensym () in
      E_letIn(P_var f,E_fun(x,name e1), E_var f)
  | E_fix(f,(x,e1)) -> (* anonymus tail-recursive functions are named *)
      E_letIn(P_var f,E_fix(f,(x,name e1)), E_var f)
  | E_if(e1,e2,e3) ->
      E_if(name e1,name e2,name e3)
  | E_letIn(p,E_fun(p2,e1),e2) ->
      E_letIn(p,E_fun(p2,name e1),name e2)
  | E_letIn((P_var f) as p,E_fix(g,(p2,e1)),e2) ->
      E_letIn(p,E_fix(g,(p2,subst_e g (E_var f) (name e1))),name e2)
  | E_letIn(p,e1,e2) ->
      E_letIn(p,name e1,name e2)
  | E_tuple(es) ->
      E_tuple(List.map name es)
  | E_app(e1,e2) ->
      E_app(name e1,name e2)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,name e1,name e2)
  | E_set(x,e1) ->
      E_set(x,name e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,name e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,name e1,name e2)
  | E_step(e1,k) ->
      E_step(name e1,k)
  | E_par(e1,e2) ->
      E_par(name e1, name e2)

let name_top (e:e) : e =
  match e with
  | E_fun(p,e) -> E_fun(p,name e)
  | E_fix(f,(p,e)) -> E_fix(f,(p,name e))
  | _ -> e

let name_pi pi =
  let ds = List.map (fun (x,e) -> (x,name_top e)) pi.ds in
  let main = name_top pi.main in
  { pi with ds ; main }
 