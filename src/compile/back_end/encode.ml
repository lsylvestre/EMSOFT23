
open Fsm_syntax

let extra_machines = ref []



let write_ xm a =
  let (write,x) = xm in
  set_ ~write x a

let notify_rdy ~result ~idle ~state_var ~rdy =
  match rdy with
  | None -> idle
  | Some xm -> seq_ (write_ xm (A_const (Bool true))) idle

let return_ ~result ~idle ~state_var ~rdy a =
  seq_ (write_ result a) @@
  notify_rdy ~result ~idle ~state_var ~rdy

let rec encode ~result ~idle ~state_var ~rdy s =
  match s with
  | S_return a -> return_ ~result ~idle ~state_var ~rdy a
  | S_continue(f,a,Some k) -> seq_ (set_ (Naming_convention.formal_param_of_fun f) a) @@
                         seq_ (set_ (Naming_convention.instance_id_of_fun f) (A_const(Enum (Naming_convention.instance_enum_const k)))) @@
                         set_ state_var (A_var f)
  | S_continue(f,a,None) ->
      seq_ (set_ (Naming_convention.formal_param_of_fun f) a) @@
      set_ state_var (A_var f)
  | S_if(a,s1,so) ->
      S_if(a, encode ~result ~idle ~state_var ~rdy s1,
              Option.map (encode ~result ~idle ~state_var ~rdy) so)
  | S_case(a,hs) ->
      S_case(a, List.map (fun (c,s) -> c,encode ~result ~idle ~state_var ~rdy s) hs)
  | S_letIn(x,a,s) ->
      S_letIn(x,a,encode ~result ~idle ~state_var ~rdy s)
  | S_set _ as s -> s
  | S_buffer_set _ as s -> s
  | S_seq(s1,s2) ->
      seq_ (encode ~result ~idle ~state_var ~rdy s1)
           (encode ~result ~idle ~state_var ~rdy s2)
  | S_fsm(id,result2,ts,s,b) ->
      let sv = Ast.gensym ~prefix:"state_var" () in
      let cp = Ast.gensym ~prefix:"compute" () in
      (*let idle = if b then (encode ~result:(Immediate,result2) ~idle ~state_var:sv ~rdy:None s) (* problem with duplication of s ? *)
                 else idle in*)
      (* todo: when b is true, we should remove the [compute] state (dead code) *)
      extra_machines := (id,(sv,cp,List.map fst ts)) :: !extra_machines;
      let ts,s = (encode_all ~result:(Immediate,result2) ~compute:cp ~state_var:sv ~rdy:(if b then None else Some (Immediate,id^"_rdy")) (ts,s)) in
      S_fsm(id,result2,ts,s,b)
  | S_print _ as s -> s

and encode_all ~result ~compute ~state_var ~rdy
 ?(idle=set_ state_var (A_var compute)) (ts,s) =
 let f s = encode ~result ~idle ~state_var ~rdy s in
 List.map (fun (x,s) -> x, f s) ts, f s
