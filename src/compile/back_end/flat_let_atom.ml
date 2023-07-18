open Combinatorial
open Fsm_syntax

let rec flat = function
| A_letIn(x,a1,a2) ->
    let bs1,a1' = flat a1 in
    let bs2,a2' = flat a2 in
    bs1@[(x,a1')]@bs2, a2'
| A_tuple(aas) ->
    let bss,aas' = List.map flat aas |> List.split in
    List.concat bss, A_tuple(aas')
| (A_const _ | A_var _ as a) -> [],a
| A_call(op,a) ->
   let bs,a' = flat a in
   bs,A_call(op,a')
| A_string_get _ as a -> (* no sub-atoms*)
   [],a
| (A_buffer_get _ | A_buffer_length _) as a -> (* no sub-atoms*)
   [],a

let s_let_bindings bs s =
  List.fold_right (fun (x,a) s -> S_letIn(x,a,s)) bs s

let flat_a a =
  let bs,a' = flat a in
  List.fold_right (fun (x,a1) a2 -> A_letIn(x,a1,a2)) bs a'

let rec flat_s = function
| S_return a ->
    let bs,a' = flat a in
    s_let_bindings bs @@
    S_return a'
| S_continue(f,a,id) ->
    let bs,a' = flat a in
    s_let_bindings bs @@
    S_continue(f,a',id)
| S_if(a,s1,so) ->
    let bs,a' = flat a in
    s_let_bindings bs @@
    S_if(a', flat_s s1,Option.map flat_s so)
| S_case(a,hs, so) ->
    let bs,a' = flat a in
    s_let_bindings bs @@
    S_case(a',List.map (fun (x,s) -> x,flat_s s) hs,Option.map flat_s so)
| S_set(w,x,a) ->
   let bs,a' = flat a in
   s_let_bindings bs @@ S_set(w,x,a')
| (S_buffer_set _) as s -> (* no sub-atoms*)
   s
| S_seq(s1,s2) ->  S_seq(flat_s s1,flat_s s2)
| S_letIn(x,a,s) ->
    let bs,a' = flat a in
    s_let_bindings bs @@ S_letIn(x,a',flat_s s)
| S_fsm(id,result,ts,s,b) ->
    S_fsm(id,result,List.map (fun (x,s) -> x,flat_s s) ts, flat_s s,b)
| S_print a -> S_print (flat_a a)

let flat_let_atom (ts,s) =
  List.map (fun (x,s) -> x,flat_s s) ts, flat_s s
