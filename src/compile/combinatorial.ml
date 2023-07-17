
open Ast

let op_combinatorial = function
| (Print)
| (To_string)
| (Random)
| (Assert) -> false
| _ -> true


let const_combinatorial c =
	match c with
  | External _ -> false
  | _ -> true

let rec combinatorial e =
  match e with
	| E_deco (e,_) -> combinatorial e
	| E_var _ -> true
	| E_const c -> const_combinatorial c
	| E_if(e1,e2,e3) -> combinatorial e1 && combinatorial e2 && combinatorial e3
	| E_app(E_var _,e1)
	| E_app(E_fix _,e1) -> false
	| E_app(E_const(Op op),e2) -> op_combinatorial op && combinatorial e2
	| E_app(e1,e2) -> false (* combinatorial e1 && combinatorial e2 *)
	| E_letIn(_,e1,e2) -> combinatorial e1 && combinatorial e2
	| E_tuple es -> List.for_all combinatorial es
	| E_fun _ | E_fix _ -> false
	| E_lastIn _ | E_set _ | E_step _ | E_par _ -> false
	| E_reg _ | E_exec _ -> false (* have an internal state *)
	| E_static_array_length _ -> true
	| E_static_array_get _ | E_static_array_set _ -> false (* static arrays are mutable *)


