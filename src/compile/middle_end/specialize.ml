open Ast
open Ast_subst


(** idea : calls are of the form [f(xc,(xc1,xc2...))] with [xi]
   possibly defined in [ds]. *)

let has_fv ds arg =
   let vs = List.map fst @@ SMap.bindings @@ Free_vars.fv arg in
   let xs = List.filter (fun x -> List.mem_assoc x ds) vs in
  (* List.iter (fun x -> Printf.printf "\n\n|--->%s\n" x) vs; *)
   List.length xs > 0

let nb_modifs = ref 0

let rec specialize ds e =
  let open Ast in
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_const _ | E_var _ -> e
  | E_fix(f,(x,e1)) -> E_fix(f,(x,specialize ds e1))
  | E_fun(x,e1) ->
      E_fun(x,specialize ds e1)
  | E_app(E_const _,e2) -> e
  | E_app(E_var f,e2) ->
      if not (List.mem_assoc f ds) then E_app(E_var f,e2) else
      if not (has_fv ds e2) then E_app(E_var f,e2) else
      begin
        incr nb_modifs;
        let p,ef = let rec find f =
                     match List.assoc_opt f ds with
                     | Some (E_fun(p,ef)) -> p,ef
                     | Some (E_var g) -> find g
                     | _ -> assert false
                   in find f
        in
        let e' =Propagation.prop_n @@ subst_p_e p e2 ef in
        e'
      end (* in ANF, we can *)
  | E_app(e1,e2) ->  E_app(specialize ds e1,e2)
  | E_if(e1,e2,e3) ->
      E_if(specialize ds e1,specialize ds e2,specialize ds e3) (* ds1, ds2 and ds3 disjoint *)
  | E_letIn(p,e1,e2) ->
      E_letIn(p,specialize ds e1,specialize ds e2)
  | E_tuple(es) ->
      let es' = List.map (specialize ds) es in
      E_tuple(es')
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,specialize ds e1,specialize ds e2)
  | E_set(x,e1) ->
      E_set(x,specialize ds e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,specialize ds e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,specialize ds e1,specialize ds e2)
  | E_step _ | E_par _ -> e (* do not transform sub-expressions under step and // *)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)

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

let rec specialize_main_n ds e =
  (* let ds,e = Propagation.propagation (ds,e) in *) (* needed, e.g. let x = (f,g) in compose(x,42) *)
  nb_modifs := 0;
  let ds',e' = (* Propagation.propagation @@ *) specialize_main ds e in
  if !nb_modifs > 0 then specialize_main_n ds' e' else (ds',e')

let specialize_pi pi =
  let rec loop acc ds =
    match ds with
    | [] ->
      let ds',main = specialize_main_n acc pi.main in
      { pi with ds=ds' ; main }
    | (x,e)::ds' ->
        let acc,e' = specialize_main_n acc e in
        loop (acc@[(x,e')]) ds'
  in loop [] pi.ds
