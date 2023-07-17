open Ast

(* ensure that the entry point and all toplevel definition [ds] have the shape (fun x -> e) *)

let fun_shape_e e =
  match e with
  | E_fun _ -> e
  | E_var f ->
     let x = gensym () in
     E_fun(P_var x,E_app(E_var f,E_var x))
  | e -> e

let fun_shape_entry_point pi =
  { pi with ds = List.map (fun (x,e) -> x, fun_shape_e e) pi.ds; main = fun_shape_e pi.main }
