open Combinatorial
open Fsm_syntax

let allow_heap_access = ref false
let allow_heap_assign = ref false

module NameC = Naming_convention

module SMap = Map.Make(String)

(** each function definition (refered by its name [f]) is associated to 
    a mapping between instance numbers of [f] and their corresponding 
    continuations (i.e., target instructions). *)
module IMap = Map.Make(Int)

let (++) = Ast.(++) 

let (+++) s1 s2 =
  IMap.fold IMap.add s1 s2

let (++>) s1 s2 =
  SMap.union (fun x s1 s2 -> Some (s1 +++ s2)) s1 s2

(** warning: (++>) has the same type than (++) but behaves differently *)

let mk_int n size =
  assert (n >= 0 && size > 0);
  (Int {value=n;tsize=TSize size})

let new_instance =
  let c  = ref 0 in
  (fun () -> incr c; !c)

let let_plug_s (a:a) (f : x -> s) : s =
  match a with
  | A_var x -> f x
  | _ ->
    let y = Ast.gensym () in
    S_letIn(y,a,f y)

(** currently, instance numbers are encoded using 12 bits. 
    TODO(enhancement): use an enumeration type instead. *)
let id_size = 12 ;;


let remove_direct_call_annot ~compute ~x s =
  let rec aux s =
  match s with
  | S_skip
  | S_setptr _
  | S_setptr_write _
  | S_buffer_set _
  | S_call _
  | S_set _ -> s
  | S_continue q ->
      if NameC.is_direct_call q then  S_continue (NameC.direct_call_name q) else s
  | S_letIn(x,a,s2) -> S_letIn(x,a,aux s2)
  | S_seq(s1,s2) ->
      (* note: s1 should not contain [continue] outside a local FSM *)
      S_seq(aux s1,aux s2)
  | S_if(a,s1,so2) ->
      S_if(a,aux s1,Option.map aux so2)
  | S_case(z,hs,so) ->
      S_case(z,List.map (fun (c,si) -> c,aux si) hs,Option.map aux so)
  | S_fsm _ -> s in
  aux s



let rec insert_kont w ~compute ~x (q,s) =  
  let rec aux s =
    match s with
    | S_continue q0 ->
        if NameC.is_tail_call q0 then S_continue (NameC.tail_call_name q0) else (
        if SMap.find_opt q0 w = None then s else (
        let q0 = if NameC.is_direct_call(q0) then NameC.direct_call_name q0 else q0 in
        (* notice that insertion has to be performed on inserted continuations *)
        aux @@
        (match SMap.find_opt q0 w with
        | None -> s
        | Some u ->
        let l = List.of_seq (IMap.to_seq u) in
        (match l with
        | [(_,k)] -> k
        | (_,k)::tt when List.for_all (fun (_,k') -> 
                             (* would be possible with physical equality ? *)
                             k = k') tt -> 
            k
        | _ -> (* even when [l] is empty ? *)
           S_case(NameC.instance_id_of_fun q0,
               (List.map (fun (m,k) ->
                          mk_int m id_size, k
                    ) l),Some S_skip)))))

    | S_if(a,s1,so) ->
        S_if(a,aux s1,Option.map (aux) so)
    | S_case(z,hs,so) -> S_case(z,List.map (fun (c,si) -> c, aux si) hs,Option.map aux so)
    | S_seq(s1,s2) ->  S_seq(aux s1,aux s2)
    | S_letIn(x,a,s) -> S_letIn(x,a,aux s)
    | S_fsm _ as s -> 
        (* ok? *)
        s 
    | S_skip
    | S_setptr _
    | S_setptr_write _
    | S_buffer_set _
    | S_call _
    | S_set _ -> s
  in 
  Some (q, remove_direct_call_annot ~compute ~x @@ aux s)

let rec to_c = function
| Ast.Unit -> Unit
| Ast.Int (n,tz) -> Int {value=n;tsize=Fsm_typing.translate_ty tz}
| Ast.Bool b -> Bool b
| Ast.String s -> String s
| Ast.C_tuple cs -> CTuple (List.map to_c cs)
| Ast.(Op _ | External _ | V_loc _) -> assert false

let to_op = function
| Ast.TyConstr ty -> TyConstr (Fsm_typing.translate_ty ty)
| Ast.Runtime p -> Runtime p
| Ast.GetTuple {pos=i;arity=n} -> GetTuple (i,n,new_tvar())
| Ast.(Wait _) -> assert false


let rec to_a (e:Ast.e) : a =
  match e with
  | Ast.E_var x -> A_var x
  | Ast.E_const c -> A_const (to_c c)
  | Ast.E_app(E_const(Op op),e) ->
      A_call(to_op op,to_a e)
  | Ast.E_if(e1,e2,e3) -> A_call(If,A_tuple [to_a e1;to_a e2;to_a e3])
  | Ast.E_tuple(es) -> A_tuple (List.map to_a es)
  | Ast.E_letIn(P_var x,e1,e2) -> A_letIn(x,to_a e1,to_a e2)
  | Ast.E_static_array_length x -> A_buffer_length(x,new_tvar())
  | _ ->
      Format.fprintf Format.std_formatter "--> %a\n"  Ast_pprint.pp_exp  e; assert false


let replace_arg e =
  match e with
  | Ast.E_fix(f,(P_var x,e1)) -> Ast_subst.subst_e x (E_var (NameC.formal_param_of_fun f)) e1
  | e -> e

(* Debug/Display *)
let [@warning "-26"] show q w =
   SMap.iter (fun x s -> Printf.printf "%s {%s : [" q x;
                        IMap.iter (fun i _ -> Printf.printf "%d," i) s;
                        Printf.printf "]}\n") w


(** [to_s ~statics g e x k] translates expression [e] to a target instruction
    setting a result in variable [x], then execution instruction [k].
    [g] is the name of the current function 
    (which is unique [TODO: ahh ?? ~> LambdaLift{let rec f() = let rec g() = f() in g() in f ()}]
    as long as [let rec] does not provide mutual recursion).  *)
let rec to_s ~statics g e x k =
  let return_ s = 
    seq_ s k 
  in
  if Combinatorial.combinatorial e then SMap.empty,SMap.empty,return_ (set_ x (to_a e)) else 
  match e with
  | E_if(a,e1,e2) ->
      (* [IF] *)
      let w1,ts1,s1 = to_s ~statics g e1 x k in
      let w2,ts2,s2 = to_s ~statics g e2 x k in
      let z = Ast.gensym () in
      (w1++>w2),(ts1 ++ ts2),S_letIn(z,to_a a,S_if(z,s1,Some s2))
  | Ast.E_match(a,hs,e_els) ->
      (* [MATCH] *)
      let ws,tss,hs' = Prelude.map_split3 (fun (c,e) -> 
                         let w,ts,s = to_s ~statics g e x k in 
                         w,ts,(to_c c,s)
                       ) hs
      in
      let ts = List.fold_left (++) SMap.empty tss in
      let w1,ts1,s1 = to_s ~statics g e_els x k in
      let w' = List.fold_left (++>) w1 ws in
      w',ts1 ++ ts,let z = Ast.gensym () in 
            S_letIn(z,to_a a, S_case(z,hs',Some s1))
  | E_letIn(P_var y,e1,e2) ->
      (* [LET] *)
      let w2,ts2,s2 = to_s ~statics g e2 x k in
      if Combinatorial.combinatorial e1 then
        w2,ts2,seq_ (set_ y (to_a e1)) s2
      else
        let w1,ts1,s1 = to_s ~statics g e1 y s2 in
        w1++>w2,ts2++ts1,s1
  | E_app(E_var f,a) ->
      if f = g then
          (* [TAIL-CALL] *)
          let s = seq_ (set_ (NameC.formal_param_of_fun f) (to_a a)) @@
                        S_continue (NameC.mark_tail_call f) in
           (SMap.empty,SMap.empty,s)
      else 
          (* [DIRECT-CALL] *)
          let n = new_instance () in
          let w = SMap.singleton f (IMap.singleton n (seq_ (set_ x (A_var (NameC.result_of_fun f))) k)) in
          let s = seq_ (set_ (NameC.instance_id_of_fun f) (A_const (mk_int n id_size))) @@
                  seq_ (set_ (NameC.formal_param_of_fun f) (to_a a)) @@
                       S_continue (NameC.mark_direct_call f) in
          (w,SMap.empty,s)
  | E_app(E_const(Op(Runtime op)),a) ->
      (* in case of instantaneous call which is not combinatorial, 
         e.g., a display function for debug  *)
      SMap.empty, SMap.empty, return_ (S_call(op,to_a a))
  
  | E_set(y,a) ->
      let w,ts,s = to_s ~statics g (E_const Unit) x k in
      (w, ts, seq_ (set_ y (to_a a)) s)
 
  | E_static_array_get(y,idx) ->
      let a = to_a idx in
      let q1 = Ast.gensym ~prefix:"pause_getI" () in
      let q2 = Ast.gensym ~prefix:"pause_getII" () in
      let ts = SMap.add q1 (S_continue q2) @@
               SMap.add q2 (return_ @@ (set_ x (A_buffer_get(y)))) SMap.empty in
      let s = seq_ (S_setptr(y,a)) (S_continue q1) in
      SMap.empty, ts, s
  | E_static_array_set(y,idx,e_upd) ->
      let a = to_a idx in
      let a_upd = to_a e_upd in
      let q = Ast.gensym ~prefix:"pause_setI" () in
      let ts = SMap.add q (seq_ (S_buffer_set(y)) (return_ @@ (set_ x (A_const Unit)))) SMap.empty  in
      let s = seq_ (S_setptr_write(y,a,a_upd)) (S_continue q) in
      SMap.empty, ts, s
  | E_lastIn(y,e1,e2) ->
     (* todo: check if e1 is indeed always an atom, or not ? *)
     let w2,ts,s2 = to_s ~statics g e2 x k in
     let s = seq_ (let_plug_s (A_call(Runtime(Not),A_var (y^"_init"))) (fun z ->
             S_if (z,
                   seq_ (S_set(y,to_a e1))
                        (S_set(y^"_init",A_const (Bool true))),
                   None))) s2 in
     (w2,ts,s)
  | E_step(e1,l) ->
      let pi = Middle_end.compile Ast.{statics;ds=[];main=e1} in
      let rdy,res,compute,(ts,s) = compile (* ~result:x*) pi in
      let s = S_fsm((Ast.gensym ~prefix:"id" ()),rdy,res,compute,ts,s,false) in
      (SMap.empty, SMap.empty, seq_ s (return_ (set_ x (A_var res))))
  | e -> Ast_pprint.pp_exp Format.std_formatter e; assert false (* todo *)

(* takes a program composed of global (non-mutual) recursive
   functions [ds] and an entry point [e] and translates it into a FSM *)
and compile ?(result=(Ast.gensym ~prefix:"result" ())) pi =
  let open Ast in
  let statics = pi.statics in
  let x = result in
  let rdy = gensym ~prefix:"rdy" () in
  let compute = gensym ~prefix:"compute" () in
  let ds = List.map (fun (x,e) -> x, replace_arg e) pi.ds in

  let ws,tss_extra,ts = Prelude.map_split3 (fun (q,e) -> 
    let k = (S_continue q) in
    let wi,tsi,si = to_s ~statics q e (NameC.result_of_fun q) k in 

    (* show q wi;*)

    wi,(q,tsi),(q,si)
  ) ds in
  
  let k = seq_ (set_ rdy (A_const (Bool true))) (S_continue compute) in
  let w0,ts0,s0 = to_s ~statics compute pi.main x k in

  (* show compute w0; *)

  let w0 = List.fold_left (++>) w0 ws in

  (* List.iter (fun (f,_) -> show f w0) ds; *)

  let ts' = List.filter_map (fun (q,s) -> insert_kont w0 ~compute ~x (q,s)) ts in
  let wmain = SMap.add compute IMap.empty w0 in
  let s' = match insert_kont wmain ~compute ~x (compute,s0) with Some (_,s) -> s | None -> s0 in
  
  let tss_extra = List.map (fun (q,ts) ->
                    List.filter_map (fun (q_aux,s) -> insert_kont w0 ~compute ~x (q_aux,s)) (SMap.bindings ts)
                         ) tss_extra in
  let ts0_extra = List.filter_map (fun (q_aux,s) -> insert_kont w0 ~compute ~x (q_aux,s)) (SMap.bindings ts0) in
  let tss_extra = List.concat (ts0_extra::tss_extra) in

  rdy,x,compute,(ts'@tss_extra,seq_ (set_ rdy (A_const (Bool false))) s')
                   
