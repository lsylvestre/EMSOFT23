(* expand each [reg] and [exec] constructs in the given epxression 
   into one equivalent expression using [var/in], [x <- e] and [step] constructs *)

open Ast

let rec encode (e:e) : e =
  let ss = encode in
  match e with
  | E_deco(e1,deco) ->
      E_deco(ss e1, deco)
  | E_const _ ->
      e
  | E_var _ -> e
  | E_fix(f,(p,e1)) ->
      E_fix (f,(p,ss e1))
  | E_fun(p,e1) ->
      E_fun (p,ss e1)
  | E_app(e1,e2) ->
      E_app(ss e1,ss e2)
  | E_tuple(es) ->
      E_tuple(List.map ss es)
  | E_letIn(p,e1,e2) ->
      E_letIn(p, ss e1, ss e2)
  | E_if(e1,e2,e3) ->
      E_if(ss e1, ss e2, ss e3)
  | E_match(e1,hs,e_els) ->
      E_match(ss e1,List.map (fun (c,e) -> c,ss e) hs,ss e_els)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,ss e1,ss e2)
  | E_set(x,e1) ->
      E_set(x,ss e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,ss e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,ss e1,ss e2)
  | E_step(e1,k) ->
      E_step(ss e1,k)
  | E_par(e1,e2) ->
      E_par(ss e1, ss e2)
  | E_reg(V (E_var f),e0) ->
      let x = gensym () in
      ss @@ E_reg(V (E_fun(P_var x, E_app(E_var f,E_var x))),e0)
  | E_reg(V (E_fun (p,e1)),e0) ->
      let x = gensym () in
      ss @@
      E_lastIn(x, e0,
      E_letIn(p, E_var x,
      E_letIn(P_unit, E_set(x,e1), E_var x)))
  | E_reg _ -> assert false (* update function must be a value *)
  | E_exec(e1,e0,k) ->
      ss @@
      let res = gensym ~prefix:"res" () in
      let y = gensym () in
      let k = gensym () in
      (* todo: use a default value "nil" to avoid the duplication of e0 *)
      E_lastIn(res,E_tuple[e0;E_const(Bool false)],
      E_letIn(P_unit, E_step(E_letIn(P_unit,(E_set(res,E_tuple[e0;E_const(Bool false)])),
                                   E_letIn(P_var y,e1,E_set(res,E_tuple[E_var y;E_const(Bool true)]))),k), E_var res))


let encode_pi (pi:pi) : pi =
  Map_pi.map encode pi
