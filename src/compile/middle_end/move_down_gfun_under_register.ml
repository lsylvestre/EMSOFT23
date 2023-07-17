
open Ast

let map_under_register f e =
  let rec aux e =
    match e with
    | E_deco _ ->
      Ast_undecorated.still_decorated e
    | E_var _ | E_const _ ->
        e
    | E_if(e1,e2,e3) ->
        E_if(aux e1, aux e2, aux e3)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,aux e1,aux e2)
    | E_app(e1,e2) ->
        E_app(aux e1,aux e2)
    | E_fun(p,e1) ->
        E_fun(p,aux e1)
    | E_fix(f,(p,e1)) ->
        E_fix(f,(p,aux e1))
    | E_tuple es ->
        E_tuple (List.map aux es)
    | E_lastIn(x,e1,e2) ->
        E_lastIn(x,aux e1,aux e2)
    | E_set(x,e1) ->
        E_set(x,aux e1)
    | E_static_array_get(x,e1) ->
        E_static_array_get(x,aux e1)
    | E_static_array_length _ ->
      e
    | E_static_array_set(x,e1,e2) ->
        E_static_array_set(x,aux e1,aux e2)
    | E_step(e1,k) ->
        E_step(f e1,k)
    | E_par(e1,e2) ->
        E_par(f e1, f e2)
    | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
  in aux e


let move_down_gfun_under_register_exp y ds e =
  let f e =
    let e_prepared_for_lambda_lifting = (Anf.anf e) in
    let rec loop e = function
    | [] -> e
    | (x,_)::_ when x = y -> e
    | (x,ex)::l -> E_letIn(P_var x,ex,loop e l)
    in
      loop e_prepared_for_lambda_lifting ds
  in
  map_under_register f e


let move_down_gfun_under_register_pi pi =
  let ds = List.map (fun (x, e) -> x, move_down_gfun_under_register_exp x pi.ds e) pi.ds in
  let main = move_down_gfun_under_register_exp (gensym()) pi.ds pi.main in
  {pi with ds ; main}
