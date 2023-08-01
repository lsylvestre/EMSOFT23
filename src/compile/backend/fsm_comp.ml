open Combinatorial
open Fsm_syntax

let allow_heap_access = ref false
let allow_heap_assign = ref false

module SMap = Map.Make(String)
module IMap = Map.Make(Int)

let (+++) s1 s2 =  IMap.union (fun _ _ v2 -> Some v2) s1 s2
let (++>) s1 s2 =  SMap.union (fun _ s1 s2 -> Some (s1 +++ s2)) s1 s2

let mk_int n size =
  assert (n >= 0 && size > 0);
  A_const (Int {value=n;tsize=TSize size})

let new_instance =
  let c  =ref 0 in
  (fun () -> incr c; !c)

let to_c = function
| Ast.Unit -> Unit
| Ast.Int (n,tz) -> Int {value=n;tsize=Fsm_typing.translate_ty tz}
| Ast.Bool b -> Bool b
| Ast.String s -> String s
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
  | Ast.E_fix(f,(P_var x,e1)) -> Ast_subst.subst_e x (E_var (Naming_convention.formal_param_of_fun f)) e1
  (*| _ -> Format.fprintf Format.std_formatter "--> %a\n"  Ast_pprint.pp_exp  e; assert false*)
  | e -> e

let access ~k ~result (address:a) ~field =
  allow_heap_access := true;
  let q = Ast.gensym ~prefix:"wait_read" () in
  let t = q, (S_if(A_call(Runtime(Not),A_var ("avm_rm_waitrequest")),
                  (seq_ (S_set("avm_rm_read",A_const (Bool false))) @@
                   seq_ (S_set(result,A_var "avm_rm_readdata")) @@ k),
                   Some (S_continue q)))
  in
  let s =
    seq_ (S_set("avm_rm_address",A_call(Compute_address,(A_tuple [address;field])))) @@
    seq_ (S_set("avm_rm_read",A_const (Bool true))) @@
    S_continue q
  in
  S_let_transitions([t],s)


let assign ~k ~result ?(value=A_const Unit) (address:a) ~field data =
  allow_heap_assign := true;
  let q = Ast.gensym ~prefix:"wait_write" () in
  let t = q, (S_if(A_call(Runtime(Not),A_var ("avm_wm_waitrequest")),
                  (seq_ (S_set("avm_wm_write",A_const (Bool false))) @@
                   seq_ (S_set(result,value)) @@
                   k),
                   Some (S_continue q)))
  in
  let s =
    seq_ (S_set("avm_wm_address",A_call(Compute_address,(A_tuple [address;field])))) @@
    seq_ (S_set("avm_rm_read",data)) @@
    seq_ (S_set("avm_rm_read",A_const (Bool true))) @@
    S_continue q
  in
  S_let_transitions([t],s)

let let_plug_s (a:a) (f : x -> s) : s =
  match a with
  | A_var x -> f x
  | _ ->
    let y = Ast.gensym () in
    S_letIn(y,a,f y)

let rec to_s ~statics ~tail x ~rdy ~k e =
  let return_ s =
    let s = seq_ s k in
    if tail then seq_ (S_set(rdy, A_const (Bool true))) s else s
  in
  let return_atom a =
    return_ (S_set(x, a))
  in
  if combinatorial e then return_atom (to_a e) else
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_fun _ | E_fix _ ->
      assert false  (* already globalized *)
  | E_const _ | E_var _ | E_tuple _ -> assert false (* already handled *)
  | E_app(E_var f,e) ->
      seq_ (set_ (Naming_convention.formal_param_of_fun f) (to_a e)) @@
      S_continue f
  | E_app(E_const (External(Array_get)),E_tuple[e1;e2]) ->
      let arr = to_a e1 in
      let idx = to_a e2 in
      access ~k ~result:x arr ~field:idx
  | E_app(E_const (External(Array_length)),e1) ->
      let arr = to_a e1 in
      access ~k ~result:x arr ~field:(A_call(Runtime(Sub),A_tuple[mk_int 0 32;mk_int 1 32]))
  | E_app(E_const (External(Array_set)),E_tuple[e1;e2;e3]) ->
      let arr = to_a e1 in
      let idx = to_a e2 in
      let v = to_a e3 in
     assign ~k ~result:x arr ~field:idx v

  | E_app(E_const(Op(Runtime op)),e1) ->
      (* in case of instantaneous call which is not combinatorial, 
         e.g., a display function for debug  *)
      return_ (S_call(op,to_a e1))
  | E_app _ ->
      Format.fprintf Format.std_formatter "--> %a\n"  Ast_pprint.pp_exp  e;
      assert false (* computed functions should be eliminated before *)
  | E_letIn(p,e1,e2) -> 
      (match p with
       | P_var y ->
          let s2 = to_s ~statics ~tail x ~rdy ~k e2 in
          if combinatorial e1 then S_letIn(y,to_a e1,s2) else
          let s1 = to_s ~statics ~tail:false y ~rdy ~k:s2 e1 in
          s1
       | _ -> assert false (* already exanded *)
      )
  | E_if(e,e1,e2) ->
     let s1 = to_s ~statics ~tail x ~rdy ~k e1 in
     let s2 = to_s ~statics ~tail x ~rdy ~k e2 in
     S_if(to_a e,s1,Some s2)
  | E_match(e1,hs,e_els) ->
      let hs' = List.map (fun (c,e) -> to_c c, to_s ~statics ~tail x ~rdy ~k e) hs in 
      let s_els = to_s ~statics ~tail x ~rdy ~k e_els in
      let y = Ast.gensym () in
      (* use a let-binding because this expression must be locally static in VHDL. TODO: enforce to be a variable in the grammar *) 
      S_letIn(y,to_a e1,
              S_case(A_var y,hs',Some s_els))
  | E_lastIn(y,e1,e2) ->
     (* todo: check if e1 is indeed always an atom, or not ? *)
     let s2 = to_s ~statics ~tail x ~rdy ~k e2 in
     seq_ (S_if (A_call(Runtime(Not),A_var (y^"_init")),
                 seq_ (S_set(y,to_a e1))
                      (S_set(y^"_init",A_const (Bool true))),
                 None)) s2
  | E_set(y,e1) ->
      return_ @@ S_set(y,to_a e1)
  | E_step(e1,k) ->
      let pi = Middle_end.compile Ast.{statics;ds=[];main=e1} in
      let rdy,res,compute,(ts,s) = compile pi in
      S_fsm((Ast.gensym ~prefix:"id" ()),rdy,res,compute,ts,s,true)
 
  | E_static_array_length(y) ->
      return_atom @@ A_buffer_length(y,new_tvar())

  | E_static_array_get(y,idx) ->
      let a = to_a idx in
      let q = Ast.gensym ~prefix:"pause" () in
      let t = q, return_atom @@ A_buffer_get(y) in
      let s = seq_ (S_setptr(y,a)) (S_continue q) in
      S_let_transitions([t],s) (* true ? don't care *)


  | E_static_array_set(y,idx,e_upd) -> 
      let a = to_a idx in
      let a_upd = to_a e_upd in
      let q = Ast.gensym ~prefix:"pause" () in
      let t = q, seq_ (S_buffer_set(y)) @@
                 return_atom (A_const Unit) in
      let s = seq_ (S_setptr_write(y,a,a_upd)) (S_continue q) in
      S_let_transitions([t],s) (* true ? don't care *)

  | E_par(e1,e2) ->
      let pi1 = Middle_end.compile Ast.{statics;ds=[];main=e1} in
      let pi2 = Middle_end.compile Ast.{statics;ds=[];main=e2} in
      let rdy1,result1,compute1,(ts1,s1) = compile pi1 in
      let rdy2,result2,compute2,(ts2,s2) = compile pi2 in
      let id1 = Ast.gensym ~prefix:"id" () in
      let id2 = Ast.gensym ~prefix:"id" () in
      let s1 = S_if(A_call(Runtime(Not),A_var (id1^"_started")), 
                    seq_ (S_set(id1^"_started",A_const (Bool true))) @@ s1,
                    None)
      in
      let s2 = S_if(A_call(Runtime(Not),A_var (id2^"_started")), 
                    seq_ (S_set(id2^"_started",A_const (Bool true))) @@ s2,
                    None)
      in
      seq_ (S_fsm(id1,rdy1,result1,compute1,ts1,s1,false)) @@
      seq_ (S_fsm(id2,rdy2,result2,compute2,ts2,s2,false)) @@
      S_if(A_call(Runtime(And),A_tuple[A_var (rdy1);A_var (rdy2)]),
          (seq_ (S_set(x,(A_tuple[A_var result1;A_var result2]))) @@
           seq_ (S_set(id1^"_started",A_const (Bool false))) @@
           seq_ (S_set(id2^"_started",A_const (Bool false))) @@
                  k),None)
  | E_reg _ | E_exec _ ->
      assert false (* already expanded *)

(* takes a program composed of global (non-mutual) recursive
   functions [ds] and an entry point [e] and translates it into a FSM *)
and compile pi =
  let open Ast in
  let statics = pi.statics in
  let x = gensym ~prefix:"result" () in
  let rdy = gensym ~prefix:"rdy" () in
  let compute = gensym ~prefix:"compute" () in
  let k = S_continue compute in
  let ds = List.map (fun (x,e) -> x, replace_arg e) pi.ds in
  let ts = List.map (fun (q,s) -> q, to_s ~statics ~tail:true x ~rdy ~k s) ds in
  rdy,x,compute,(ts,seq_ (set_ rdy (A_const (Bool false))) @@
                    to_s ~statics ~tail:true x ~rdy ~k pi.main)


