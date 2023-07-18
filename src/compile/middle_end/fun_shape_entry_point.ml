open Ast

(* ensure that the entry point and all toplevel value [ds] defined as alias 
   have the shape (fun x -> e). *)

let fun_shape_e (e:e) : e =
  match e with
  | E_fun _ -> e
  | E_var f ->
     let x = gensym () in
     E_fun(P_var x,E_app(E_var f,E_var x)) (* simple eta-expansion *)
  | e -> e

let fun_shape_entry_point (pi:pi) : pi =
  let ds = List.map (fun (x,e) -> x, fun_shape_e e) pi.ds in
  let main = fun_shape_e pi.main in
  { pi with ds ; main }
