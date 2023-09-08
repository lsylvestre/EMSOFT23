open Ast
open Ast_subst

 (** propagate combinational expressions bound to a name used zero or one time *)
let flag_propagate_combinational_linear = ref true


(* [linear_bindings e] produces the set of the names that are locally defined in [e]
  occurring (as variable) exactly once *)
let linear_bindings (e:e) : set =
  let h = Hashtbl.create 10 in
  let rec aux = function
  | E_deco(e,_) ->
      aux e
  | E_var x ->
      (match Hashtbl.find_opt h x with
      | None -> Hashtbl.add h x true
      | Some v -> Hashtbl.replace h x false)
  | E_const _ ->
      ()
  | E_if(e1,e2,e3) ->
      aux e1; aux e2; aux e3
  | E_match(e1,hs,e_els) ->
      aux e1; List.iter (fun (_,ei) -> aux ei) hs; aux e_els
  | E_letIn(p,e1,e2) ->
      aux e1; aux e2
  | E_app(e1,e2) ->
      aux e1; aux e2
  | E_fun(p,e1) ->
      aux e1
  | E_fix(f,(p,e)) ->
      aux e
  | E_tuple(es) ->
      List.iter aux es
  | E_reg(V ev, e0) ->
      aux ev; aux e0
  | E_exec(e1,e2,_k) ->
      aux e1; aux e2
   | E_lastIn(x,e1,e2) ->
      aux e1; aux e2
  | E_set(x,e1) ->
      aux e1
  | E_static_array_get(x,e1) ->
      aux e1
  | E_static_array_length(x) ->
      ()
  | E_static_array_set(x,e1,e2) ->
      aux e1; aux e2
  | E_step(e1,_k) ->
      aux e1
  | E_par(e1,e2) ->
      aux e1; aux e2
  in
  aux e;
  let keep x b acc = 
    if b then SMap.add x () acc else acc
  in
  Hashtbl.fold keep h SMap.empty



let rec simple_atom e =
  match e with 
    | E_var _ | E_const _ -> true
    | E_tuple es -> List.for_all simple_atom es
    | _ -> false


let propagation ~env e =
  let propageable e = 
    if !flag_propagate_combinational_linear then Combinatorial.combinatorial e else 
    simple_atom e in
  let rec prop e =
    match e with
    | E_deco _ ->
        Ast_undecorated.still_decorated e
    | E_var _ | E_const _ ->
        e
    | E_if(e1,e2,e3) ->
        E_if(prop e1,prop e2,prop e3)
    | E_match(e,hs,e_els) ->
        E_match(prop e,List.map (fun (c,e) -> c,prop e) hs,prop e_els)
    | E_letIn(P_var x as p,e1,e2) ->
        if SMap.mem x env && propageable e1
        then prop (subst_p_e p (prop e1) e2) 
        else E_letIn(p,prop e1,prop e2)
    | E_letIn _ -> assert false (* already expanded, see matching.ml *)
    | E_fun(x,e) ->
        E_fun(x,prop e)
    | E_fix(f,(x,e)) ->
        E_fix(f,(x,prop e))
    | E_app(E_const(Op(GetTuple{pos;arity})),E_tuple vs) ->
        prop (List.nth vs pos)
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
  in prop e

let rec toplevel_constant_folding ds e =
  match ds with
  | [] -> e
  | (x,(E_const _| E_var _ as ec))::ds' -> toplevel_constant_folding ds' (subst_e x ec e)
  | _::ds' -> toplevel_constant_folding ds' e

let propagation_aux ds e =
  let e_all = (toplevel_constant_folding ds e)
  in 
  let env = linear_bindings e_all in
  propagation ~env e_all

let propagation_pi pi =
  Map_pi.map (propagation_aux pi.ds) pi
