

let formal_param_of_fun f =
  (f^"_arg")

(* instance_id_of_fun *)
let instance_id_of_fun x =
  x^"_id"


let instance_enum_const k =
  "I"^string_of_int k


let state_var_type st =
  "t_"^st

(* type of the instances I1, ... In associated to the automaton whose state variable is st *)
let instances_type st =
  "t_"^st
