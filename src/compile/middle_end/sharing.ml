open Ast

module IMap = Map.Make(Int)

let (+++) s1 s2 =  IMap.union (fun _ _ v2 -> Some v2) s1 s2
let (++>) s1 s2 =  SMap.union (fun _ s1 s2 -> Some (s1 +++ s2)) s1 s2

let insert_kont w (x,e) =
  match SMap.find_opt x w with
  | None -> Some(x,e)
  | Some u ->
  let n = IMap.cardinal u in
  let l = List.of_seq (IMap.to_seq u) in
  if n <= 0 then None else
  let rec aux e =
    match e with
    | E_deco _ ->
      Ast_undecorated.still_decorated e
    | E_const _ | E_var _ | E_tuple _ | E_app(E_const _,_)
    | E_set _
    | E_static_array_get _
    | E_static_array_length _
    | E_static_array_set _ ->
        E_match(E_var (Naming_convention.instance_id_of_fun x),
               (List.map (fun (n,k) ->
                           (* Enum (Naming_convention.instance_enum_const n)*)
                          Int(n,Types.unknown()),k e
                  ) l),E_app(E_const(Op(Runtime(Assert))),E_const(Bool false)))

  | E_fix(f,(p,e1)) ->
      E_fix (f,(p,aux e1))
  | E_fun(p,e1) ->
      E_fun (p,aux e1)
  | E_app(e1,e2) ->
      E_app(e1, e2)
  | E_reg(V ev,e0) ->
      E_reg(V (aux ev), aux e0)
  | E_exec(e1,e2,k) ->
      E_exec(aux e1,aux e2,k)
  | E_letIn(p,e1,e2) ->
      E_letIn(p, e1, aux e2)
  | E_if(e1,e2,e3) ->
      E_if(e1, aux e2, aux e3)
  | E_match(e,hs,e_els) ->
      E_match(e,List.map (fun (c,e) -> c,aux e) hs,aux e_els)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,aux e1, aux e2) (* aux ok ? *)
  | E_step(e1,k) ->
      E_step(aux e1,k)
  | E_par(e1,e2) ->
      E_par(aux e1, aux e2)
  in
  Some(x,aux e)

let new_instance =
  let c  =ref 0 in
  (fun () -> incr c; !c)


(* w : map(f |-> map(id |-> (x,ks)) *)

(** [share e] puts expression [e] in share-form *)

let rec share tail_env (k: e -> e) (e:e) : (_ * e) =
  let open Ast in
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_const _ | E_var _ | E_tuple _ | E_app(E_const _,_) 
  | E_set _    
  | E_static_array_get _
  | E_static_array_length _ 
  | E_static_array_set _
  | E_step _ ->
      SMap.empty, k e
  | E_app(E_var f as ef,xc) ->
      if SMap.mem f tail_env 
      then SMap.empty,E_app(E_var f,E_tuple[xc;E_var (Naming_convention.instance_id_of_fun f)]) else
      let id = new_instance () in
      let w = SMap.singleton f (IMap.singleton id k) in
      w,E_app(ef,E_tuple[xc;E_const(Int(id,Types.unknown()))])
  | E_app _ -> 
      assert false (* already expanded *)
  | E_fix(f,(p,e1)) ->
      let tail_env' = SMap.add f () tail_env in
      let w1,e1' = share tail_env' k e1 in
      w1,E_fix(f,(P_tuple[p;P_var (Naming_convention.instance_id_of_fun f)],e1'))
  | E_fun(x,e1) ->
      let w1,e1' = share tail_env k e1 in
      w1,E_fun(x,e1')
  | E_if(xc,e1,e2) ->
      let w1,e1' = share tail_env k e1 in
      let w2,e2' = share tail_env k e2 in
      w1++>w2,E_if(xc,e1',e2')
  | E_match(xc,hs,e_els) ->
      let ws,hs' = List.split @@ List.map (fun (c,e) -> let w,e' = share tail_env k e in w,(c,e')) hs in
      let w,e_els' = share tail_env k e_els in
      List.fold_left (++>) w ws, E_match(xc,hs',e_els')
  | E_letIn(p,e1,e2) ->
      let w2,e2' = share tail_env k e2 in
      let w3,e3' = share tail_env (fun ex -> E_letIn(p,ex,e2')) e1 in
      w2++>w3,e3'
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)
  | E_lastIn(x,e1,e2) ->
      (* e1 must be combinatorial ? *)
      let w2,e2' = share tail_env k e2 in
      w2,E_lastIn(x,e1,e2')
  | E_par(e1,e2) ->
      (* will be compiled elswhere *)
      SMap.empty,E_par(e1,e2)


(*  *)
let sharing_pi (Ast.{statics;ds;main=e} as pi) =
  let rec aux w rts ds =
    match ds with
    | [] -> let w',s = share SMap.empty (fun xe -> xe) e in
             (w' ++> w),(List.rev rts,s)
    | (x,e)::ds' ->
        let w',s = share SMap.empty (fun xe -> xe) e in
        aux (w' ++> w) ((x,s)::rts) ds'
  in
  let w,(ds',e') = aux SMap.empty [] ds in
  let ds' = List.filter_map (insert_kont w) ds' in
  {pi with ds=ds'; main=e'}


