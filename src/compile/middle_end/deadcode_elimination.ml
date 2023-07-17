open Ast

(* assume that each function name is unique *)

let rec ds_inclusion ds ds_keeped =
  match ds_keeped with
  | [] -> []
  | (x,e)::ds_keeped' ->
      let ds_called_by_e = ds_inclusion ds ds_keeped' in
      (* todo: and if [ds_called_by_e] contain x ? *)
      (x,e)::ds_called_by_e

and deadcode_elim_ds (ds,e) =
  let xs = Free_vars.fv e in
  let ds_called_by_e = List.filter (fun (y,_) -> Ast.SMap.mem y xs) ds in
  ds_inclusion ds ds_called_by_e


let deadcode_elimination_pi pi =
  let ds = deadcode_elim_ds (pi.ds,pi.main) in
  { pi with ds }

