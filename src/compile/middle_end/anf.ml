open Ast
open Ast_subst
open Combinatorial


(**

  The language in its ANF form is defined as follow:

  ;; expression
  e ::= xc
      | fun x -> e
      | fix (fun x -> e)
      | xc(xc)
      | if xc then e else e
      | let p = e in e
      | reg x last xc
      | (exec e xc)^id

xc := (xc,xc ... xc)
    | c
    | x

*)

let rec is_xc e =
  match un_deco e with
  | E_deco(e,_) -> is_xc e
  | E_var _ | E_const _ -> true
  | E_tuple(es) -> List.for_all is_xc es
  | _ -> false

let plug e context =
  if is_xc e then context e else
  let x = gensym ~prefix:"anf" () in
  E_letIn(P_var x,e,context (E_var x))

let rec plug_n_aux aas es context =
  match es with
  | [] -> context (List.rev aas)
  | e::es' -> plug e @@ fun v -> plug_n_aux (v::aas) es' context

let plug_n es context = plug_n_aux [] es context

let rec anf e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ -> e
  | E_fun(x,e1) ->
      E_fun(x,anf e1)
  | E_fix(f,(x,e1)) ->
      E_fix(f,(x,anf e1))
  | E_if(e1,e2,e3) ->
      plug (anf e1) @@ fun xc ->
      E_if(xc,anf e2,anf e3)
  | E_letIn(P_var f,(E_fix(g,(p,e1))),e2) when f <> g ->
      assert (not (pat_mem f p) && not (pat_mem g p));
      anf @@ E_letIn(P_var f,E_fix(f,(p,subst_e g (E_var f) e1)),e2)
  | E_letIn(P_var x,(E_var _ as e1),e2) ->
      anf @@ subst_e x e1 e2
  | E_letIn(p,e1,e2) ->
      E_letIn(p,anf e1,anf e2)
  | E_tuple(es) ->
      plug_n (List.map anf es) @@
      fun xs -> E_tuple(xs)
  | E_app(E_const _ as ec,e2) ->
      plug (anf e2) @@ fun x2 ->
      E_app(ec,x2)
  | E_app(e1,e2) ->
      plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_app(xc1,xc2)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,anf e1,anf e2)
  | E_set(x,e1) ->
      plug (anf e1) @@ fun xc1 ->
      E_set(x,xc1)
  | E_static_array_get(x,e1) ->
      plug (anf e1) @@ fun xc1 ->
      E_static_array_get(x,xc1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      plug (anf e1) @@ fun xc1 ->
      plug (anf e2) @@ fun xc2 ->
      E_static_array_set(x,xc1,xc2)
  | E_step(e1,k) ->
      E_step(anf e1,k)
  | E_par(e1,e2) ->
      E_par(anf e1, anf e2)

let rec in_anf e : bool =
  match e with
  | E_deco(e1,_) ->
      in_anf e1
  | E_var _ | E_const _ -> true
  | E_fun(_,e1) ->
      in_anf e1
  | E_fix(_,(_,e1)) ->
      in_anf e1
  | E_if(e1,e2,e3) ->
      is_xc e1 && in_anf e2 && in_anf e3
  | E_letIn(_,e1,e2) ->
      in_anf e1 && in_anf e2
  | E_tuple(es) ->
      List.for_all is_xc es
  | E_app(e1,e2) ->
      is_xc e1 && is_xc e2
  | E_reg _ | E_exec _ ->
       assert false (* already expanded *)
  | E_lastIn(x,e1,e2) ->
      in_anf e1 && in_anf e2
  | E_set(x,e1) ->
      is_xc e1
  | E_static_array_get(x,e1) ->
      is_xc e1
  | E_static_array_length _ ->
      true
  | E_static_array_set(x,e1,e2) ->
      is_xc e1 && is_xc e2
  | E_step(e1,_) ->
      in_anf e1
  | E_par(e1,e2) ->
      in_anf e1 && in_anf e2

let anf_pi pi =
  Map_pi.map anf pi

let in_anf_pi pi =
  Map_pi.for_all in_anf pi
