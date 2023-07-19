open Ast

(* assumes that each function is global, each function name being unique *)

let ds_called_by ds (x,e) =
  let xs = Free_vars.fv e in
  SMap.filter (fun y _ -> Ast.SMap.mem y xs && x <> y) ds

let rec deadcode_elim_ds ds (x,e) =
  let ds_keeped = ds_called_by ds (x,e) in
  SMap.fold (fun x e acc ->
               deadcode_elim_ds ds (x,e) ++
               SMap.add x e acc) ds_keeped SMap.empty
 
let deadcode_elimination_pi pi =
  let name_entry_point = gensym () in 
  (** this name does not appear in the program after transformation. 
      It is used internally to avoid code-duplication: the entry_point 
      is treated in the same way as other declarations [ds] *)
  let ds_keeped = deadcode_elim_ds (smap_of_list @@ pi.ds) (name_entry_point,pi.main) in
  let ds = List.filter (fun (x,_) -> SMap.mem x ds_keeped) pi.ds in
  { pi with ds }

