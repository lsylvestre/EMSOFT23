
open Ast

let fv ?(xs=SMap.empty) e =
  let open Ast in
  let rec aux xs = function
  | E_deco(e,_) ->
      aux xs e
  | E_var x ->
      if SMap.mem x xs then SMap.empty else SMap.singleton x ()
  | E_const _ ->
      SMap.empty
  | E_if(e1,e2,e3) ->
      aux xs e1 ++ aux xs e2 ++ aux xs e3
  | E_letIn(p,e1,e2) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs e1 ++ aux xs' e2
  | E_app(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_fun(p,e) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e
  | E_fix(f,(p,e)) ->
      let ys = vars_of_p p in
      let xs' = SMap.add f () @@ (xs++ys) in
      aux xs' e
  | E_tuple(es) ->
      List.fold_left (fun acc e -> acc ++ aux xs e) SMap.empty es
  | E_reg(V ev, e0) ->
      aux xs ev ++ aux xs e0
  | E_exec(e1,e2,_k) ->
      (* _k is in a different name space than variables *)
      aux xs e1 ++ aux xs e2
   | E_lastIn(x,e1,e2) ->
      let xs' = SMap.add x () xs in
      aux xs e1 ++ aux xs' e2
  | E_set(x,e1) ->
      aux (SMap.add x () xs) e1
  | E_static_array_get(x,e1) ->
      let xs' = (SMap.add x () xs) in
      aux  xs' e1
  | E_static_array_length(x) ->
      SMap.add x () xs
  | E_static_array_set(x,e1,e2) ->
      let xs' = (SMap.add x () xs) in
      aux  xs' e1 ++ aux  xs' e2
  | E_step(e1,_k) ->
      (* _k is in a different name space than variables *)
      aux xs e1
  | E_par(e1,e2) ->
      aux xs e1 ++ aux xs e2
  in
  aux xs e
