open Ast
open Ast_subst

(* remove simulation primitives like print and assert *)

let clean_exp ~no_print ~no_assert e =
  let rec clean e =
    match e with
    | E_deco(e,loc) ->
        E_deco(clean e,loc)
    | E_var _ | E_const _ -> e
    | E_fun(x,e1) ->
        E_fun(x,clean e1)
    | E_fix(f,(x,e1)) ->
        E_fix(f,(x, clean e1))
    | E_if(e1,e2,e3) ->
        E_if(clean e1,clean e2,clean e3)
    | E_match(e,hs,e_els) ->
        E_match(clean e,List.map (fun (c,e) -> c,clean e) hs,clean e_els)
    | E_letIn(p,E_fun(p2,e1),e2) ->
        E_letIn(p,E_fun(p2,clean e1),clean e2)
    | E_letIn((P_var f) as p,E_fix(g,(p2,e1)),e2) ->
        E_letIn(p,E_fix(g,(p2,subst_e g (E_var f) (clean e1))),clean e2)
    | E_letIn(p,e1,e2) ->
        E_letIn(p,clean e1,clean e2)
    | E_tuple(es) ->
        E_tuple(List.map clean es)
    | E_app(e1,e2) ->  
        let opt = match un_annot e1 with
                  | E_const(Op(Runtime(Print | Print_string | Print_int | Print_newline))) ->
                       if no_print then Some (E_const(Unit)) else None
                  | E_const(Op(Runtime(Assert))) ->
                       if no_print then Some (E_const(Unit)) else None
                  | _ -> None in 
        (match opt with
        | None -> E_app(clean e1,clean e2)
        | Some e0 -> e0)
    | E_reg(V ev,e0) ->
        E_reg(V (clean ev), clean e0)
    | E_exec(e1,e2,k) ->
        E_exec(clean e1,clean e2,k)
    | E_lastIn(x,e1,e2) ->
        E_lastIn(x,clean e1,clean e2)
    | E_set(x,e1) ->
        E_set(x,clean e1)
    | E_static_array_get(x,e1) ->
        E_static_array_get(x,clean e1)
    | E_static_array_length _ ->
        e
    | E_static_array_set(x,e1,e2) ->
        E_static_array_set(x,clean e1,clean e2)
    | E_step(e1,k) ->
        E_step(clean e1,k)
    | E_par(e1,e2) ->
        E_par(clean e1, clean e2)
  in clean e

let clean_pi ~no_print ~no_assert pi =
  let ds = List.map (fun (x,e) -> (x,clean_exp ~no_print ~no_assert e)) pi.ds in
  let main = clean_exp ~no_print ~no_assert pi.main in
  { pi with ds ; main }
 