open Ast


(* value propagation, including inlining and constant/copy propagation *)

let rec inline env e =
  match e with
  | E_deco(e,d) ->
      E_deco(inline env e,d)
  | E_const _ ->
      e
  | E_fun(p,e) ->
      let xs = vars_of_p p in
      E_fun(p,inline (SMap.filter (fun x _ -> not (SMap.mem x xs)) env) e)
  | E_fix(f,(p,e)) ->
      let xs = vars_of_p p in
      E_fix(f,(p,inline (SMap.filter (fun x _ -> not (SMap.mem x xs)) env) e))
  | E_var x ->
      (match SMap.find_opt x env with
       | None -> e
       | Some v -> v)
  | E_app(e1,e2) ->
      E_app(inline env e1,inline env e2)
  | E_tuple es ->
      E_tuple (List.map (inline env) es)
  | E_if(e1,e2,e3) ->
      E_if(inline env e1,inline env e2,inline env e3)
  | E_letIn(p,e1,e2) ->
      let e1 = inline env e1 in
      let r = bindings p e1 in
      let r1, r2 = SMap.partition (fun _ e -> evaluated e || is_variable e) r in
      let env' = env++r1 in
      let xs,es = List.split @@ SMap.bindings r2 in
      if xs = [] then inline env' e2 else
      let ps = List.map (fun x -> P_var x) xs in
      List.fold_right2 (fun p e acc -> E_letIn(p,e,acc)) ps es (inline env' e2)
      (* E_letIn(m,group_ps ps,group_es es,inline env' e2) *)
  | E_reg(V ev,e0) ->
      E_reg(V (inline env ev),inline env e0)
  | E_exec(e1,e2,x) ->
      E_exec(inline env e1,inline env e2,x)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,inline env e1,inline env e2)
  | E_set (x,e1) ->
      E_set (x, inline env e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,inline env e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,inline env e1,inline env e2)
  | E_step (e1,k) ->
      E_step (inline env e1,k)
  | E_par (e1,e2) ->
      E_par (inline env e1,inline env e2)


let normalize e =
  (* Ast_rename.rename_e @@ *)
  Instantiate.instantiate (inline Ast.SMap.empty (Ast_rename.rename_e e))
