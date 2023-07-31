open Ast
open Ast_subst

let applyed e =
  let open Ast in
  let rec aux xs = function
  | E_deco(e,_) ->
      aux xs e
  | E_var x ->
      SMap.empty
  | E_const _ ->
      SMap.empty
  | E_if(e1,e2,e3) ->
      aux xs e1 ++ aux xs e2 ++ aux xs e3
  | E_match(e1,hs,e_els) ->
      aux xs e1 ++ 
      List.fold_left (fun acc (c,ei) -> (acc ++ aux xs ei)) SMap.empty hs ++ 
      aux xs e_els
  | E_letIn(p,e1,e2) ->
      let ys = vars_of_p p in
      let xs' = xs ++ ys in
      aux xs e1 ++ aux xs' e2
  | E_app(E_var x,e2) ->
      if SMap.mem x xs then SMap.empty else SMap.singleton x ()
  | E_app(e1,e2) ->
      aux xs e1 ++ aux xs e2
  | E_fun(p,e1) ->
      let ys = vars_of_p p in
      let xs' = xs++ys in
      aux xs' e1
  | E_fix(f,(p,e)) ->
      let ys = vars_of_p p in
      let xs' = SMap.add f () @@ (xs++ys) in
      aux xs' e
  | E_tuple(es) ->
      List.fold_left (fun acc ei -> acc ++ aux xs ei) SMap.empty es
  | E_reg(V ev, e0) ->
      aux xs ev ++ aux xs e0
  | E_exec(e1,e2,_) ->
      aux xs e1 ++ aux xs e2
   | E_lastIn(x,e1,e2) ->
      let xs' = SMap.add x () xs in
      aux xs e1 ++ aux xs' e2
  | E_set(x,e1) ->
      SMap.add x () @@ aux xs e1
  | E_static_array_get(x,e1) ->
      SMap.add x () @@ aux xs e1
  | E_static_array_length(x) ->
      SMap.singleton x ()
  | E_static_array_set(x,e1,e2) ->
      SMap.add x () @@ (aux xs e1 ++ aux xs e2)
  | E_step(e1,_) ->
      aux xs e1
  | E_par(e1,e2) ->
      aux xs e1 ++ aux xs e2
  in
  aux SMap.empty e


let hof x e =
  match e with
  | E_fun(p,e) -> let xs = applyed e in
                   vars_of_p p |> SMap.filter (fun x _ -> SMap.mem x xs) |> SMap.is_empty |> not

  | E_fix(g,(p,e)) ->
  let xs = applyed e in
                   vars_of_p p |> SMap.filter (fun y _ -> y <> g && y <> x && SMap.mem y xs) |> SMap.is_empty |> not
  | _ -> false

let specialize ds e =
  let open Ast in
  let rec spec funcs e =
    match e with
    | E_deco _ ->
        Ast_undecorated.still_decorated e
    | E_const _ | E_var _ -> e
    | E_fix(f,(x,e1)) -> E_fix(f,(x,spec funcs e1))
    | E_fun(x,e1) ->
        E_fun(x,spec funcs e1)
    | E_app(E_const _,e2) -> e
    | E_app(E_var f,xc) ->
        (match SMap.find_opt f funcs with
        | None -> e
        | Some (E_fun(p,e1)) -> Ast_subst.subst_p_e p xc e1
        | Some E_fix _ ->
             (* TODO: recursive HOF not yet supported *)
            assert false
        | Some _ ->
            assert false (* ill typed *)
      )
    | E_app(e1,e2) ->
        E_app(spec funcs e1,e2)
    | E_if(e1,e2,e3) ->
        E_if(spec funcs e1,spec funcs e2, spec funcs e3) (* ds1, ds2 and ds3 disjoint *)
    | E_match(e,hs,e_els) ->
        E_match(spec funcs e,List.map (fun (c,e) -> c,spec funcs e) hs,spec funcs e_els)
    | E_letIn(P_var x,((E_var _ | E_tuple _) as e1),e2) ->
        (* copy propagation to remove aliasing of global functions *)
        spec funcs (Ast_subst.subst_e x e1 e2)
    | E_letIn(P_var x,((E_fun _| E_fix _) as e1),e2) ->
        let e1' = spec funcs e1 in
        if hof x e1 then spec (SMap.add x e1' funcs) e2 else
        E_letIn(P_var x,e1',spec funcs e2)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,spec funcs e1,spec funcs e2)
    | E_tuple(es) ->
        let es' = List.map (spec funcs) es in
        E_tuple(es')
    | E_lastIn(x,e1,e2) ->
        E_lastIn(x,spec funcs e1,spec funcs e2)
    | E_set(x,e1) ->
        E_set(x,spec funcs e1)
    | E_static_array_get(x,e1) ->
        E_static_array_get(x,spec funcs e1)
    | E_static_array_length _ ->
        e
    | E_static_array_set(x,e1,e2) ->
        E_static_array_set(x,spec funcs e1,spec funcs e2)
    | E_step _ | E_par _ -> e (* do not transform sub-expressions under step and // *)
    | E_reg _ | E_exec _ ->
        assert false (* already expanded *)
  in spec SMap.empty e



let rec list_update (x,v) = function
| [] -> []
| (y,v')::l -> if x = y then (x,v)::l else (y,v')::list_update (x,v) l

let specialize_main ds e =
  let rec loop acc_ds = function
  | [] ->
      acc_ds,specialize acc_ds e
  | (x,ex)::ds' ->
     let e' = specialize ds ex in
     loop (list_update (x,e') acc_ds) ds'
  in loop ds ds

let specialize_pi pi =
  let rec loop acc ds =
    match ds with
    | [] ->
      let ds',main = specialize_main acc pi.main in
      { pi with ds=ds' ; main }
    | (x,e)::ds' ->
        let acc,e' = specialize_main acc e in
        loop (acc@[(x,e')]) ds'
  in loop [] pi.ds
