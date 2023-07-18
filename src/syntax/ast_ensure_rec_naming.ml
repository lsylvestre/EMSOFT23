open Ast
open Ast_subst

(* given an AST, ensures that definition of tail-recursive functions
   are of the form [let f = fix f (fun x -> e) in e1] *)

let rec rename f (e:e) : e =
  match e with
  | E_deco(e,d) -> E_deco(rename f e,d)
  | E_app(E_const(Op(TyConstr ty)),e) -> E_app(E_const(Op(TyConstr ty)),rename f e)
  | E_fix(g,(p,e)) -> E_fix(f,(p,subst_e g (E_var f) e))
  | e -> e

let rec ensure_rec_naming_e e =
  let ss = ensure_rec_naming_e in
  match e with
  | E_deco(e1,deco) ->
      E_deco(ss e1, deco)
  | E_const _ ->
      e
  | E_var _ -> e
  | E_fix(f,(p,e1)) ->
      E_fix(f,(p,ss e1))
  | E_fun(p,e1) ->
      E_fun (p,ss e1)
  | E_app(e1,e2) ->
      E_app(ss e1,ss e2)
  | E_reg(V ev,e0) ->
      E_reg(V (ss ev), ss e0)
  | E_exec(e1,e2,k) ->
      (* [k] is in a different name space than variables *)
      E_exec(ss e1,ss e2,k)
  | E_tuple(es) ->
      E_tuple(List.map ss es)
  | E_letIn(p,e1,e2) ->
      (match un_annot e1,p with
      | E_fix _,P_var f -> E_letIn(P_var f,rename f (ss e1),ss e2)
      | _ -> E_letIn(p,ss e1,ss e2))
  | E_if(e1,e2,e3) ->
      E_if(ss e1, ss e2, ss e3)
  | E_match(e,hs,e_els) ->
      E_match(ss e, List.map (fun (c,e) -> c,ss e) hs, ss e_els)
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,ss e1,ss e2)
  | E_set(x,e1) ->
      E_set(x,ss e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,ss e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,ss e1,ss e2)
  | E_step(e1,k) ->
      (* [k] is in a different name space than variables *)
      E_step(ss e1,k)
  | E_par(e1,e2) ->
      E_par(ss e1, ss e2)

let ensure_rec_naming_pi pi =
  let ds = List.map (fun (x,e) -> x,ensure_rec_naming_e e) pi.ds in
  let main = ensure_rec_naming_e pi.main in
  { pi with ds ; main }
