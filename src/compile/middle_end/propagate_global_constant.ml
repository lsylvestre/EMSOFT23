open Ast
open Ast_subst

let is_fun (_,e) =
  match e with
  | E_fix _ | E_fun _ -> true 
  | _ -> false

(* propagate toplevel declaration which are not functions.
   It is not an optimisation : it is required to then target
   a FSM language. *)

let propagate_global_constant_pi pi =
  let rec aux acc ds e =
    match ds with
    | [] -> {pi with ds=List.rev acc; main=e}
    | ((x1,e1) as d)::ds' ->
        if is_fun d then aux (d::acc) ds' e else
        (* substitution is here inefficient *)
        aux acc (List.map (fun (xi,ei) -> xi,subst_e x1 e1 ei) ds') (subst_e x1 e1 e)
  in aux [] pi.ds pi.main

