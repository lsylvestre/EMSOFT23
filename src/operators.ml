
let flag_no_assert = ref false
let flag_no_print = ref false


(* instantaneous primitives which do not require an encoding *)
type op =
  | Add | Sub | Mult | Div | Mod
  | Lt | Le | Gt | Ge | Eq | Neq
  | And | Or | Xor | Not | Abs
  | Land | Lor | Lxor | Lsl | Lsr | Asr

  | Resize_int of int
  | String_length

  (* for simulation only *)
  | Print | Print_string | Print_int | Print_newline | Assert


let combinatorial p =
  match p with
  | Print | Print_string | Print_int | Print_newline | Assert -> false
  | _ -> true

let ty_op op =
  let open Types in
  match op with
  | Abs ->
      let sz = unknown() in
      let t = tint sz in
      t ==> t
  | Add|Sub|Mult|Div|Mod|Land|Lor|Lxor|Lsl|Lsr|Asr ->
      let sz = unknown() in
      let t = tint sz in
      (T_tuple[t;t]) ==> t
  | Lt|Gt|Le|Ge|Eq|Neq ->
      let sz = unknown() in
      let t = tint sz in
      (T_tuple[t;t]) ==> tbool
  | Not ->
      tbool ==> tbool
  | And|Or|Xor ->
      (T_tuple[tbool;tbool]) ==> tbool
  | Resize_int k ->
      let sz = unknown() in
      (tint sz) ==> tint (T_size k)
  | Print ->
      (unknown()) ==> tunit
  | Print_string ->
      (T_string (unknown())) ==> tunit
  | Print_int ->
      (tint (unknown())) ==> tunit
  | Print_newline ->
      tunit ==> tunit
  | Assert ->
      tbool ==> tunit
  | String_length ->
      (* enforce result to be a 16-bit integer *)
      let tz_int = T_size 16 in
      (T_string(unknown ())) ==> (tint tz_int)
  
(** pretty printer for operators *)
let pp_op fmt (op:op) : unit =
  Format.fprintf fmt "%s" @@
  match op with
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Le -> "<="
  | Lt -> "<"
  | Ge -> ">="
  | Gt -> ">"
  | Eq -> "=="
  | Neq -> "<>"
  | Not -> "not"
  | Abs -> "abs"
  | And -> "&&"
  | Or -> "or"
  | Mod -> "mod"
  | Div -> "/"
  | Xor  -> "xor"
  | Land -> "land"
  | Lor -> "lor"
  | Lxor -> "lxor"
  | Lsl -> "lsl"
  | Lsr -> "lsr"
  | Asr -> "asr"
  | Resize_int k -> "resize_int<" ^ string_of_int k ^ ">"
  | Print -> "print"
  | Print_string -> "print_string"
  | Print_int -> "print_int"
  | Print_newline -> "print_newline"
  | Assert -> "assert"
  | String_length -> "string_length"



(** code generator for operators *)
let gen_op fmt (op:op) pp a : unit =
  let open Format in
  let funcall fmt s = fprintf fmt "%s(%a)" s pp a in
  let procall fmt s = fprintf fmt "%s(%a)" s pp a in
  let skip_when b fmt f a =
    if b then fprintf fmt "mixc_skip(misc_unit)" 
    else f fmt a in
  match op with
  | Add -> funcall fmt "mixc_add"
  | Sub -> funcall fmt "mixc_sub"
  | Mult -> funcall fmt "mixc_mult"
  | Eq -> funcall fmt "mixc_eq"
  | Neq -> funcall fmt "mixc_neq"
  | Lt ->  funcall fmt "mixc_lt"
  | Le -> funcall fmt "mixc_le"
  | Gt -> funcall fmt "mixc_gt"
  | Ge -> funcall fmt "mixc_ge"
  | And -> funcall fmt "mixc_and"
  | Or -> funcall fmt "mixc_or"
  | Xor -> funcall fmt "mixc_xor"
  | Not -> funcall fmt "mixc_not"
  | Abs -> funcall fmt "mixc_abs"
  | Div -> funcall fmt "mixc_div"
  | Mod -> funcall fmt "mixc_mod"
  | Land -> funcall fmt "mixc_land"
  | Lor -> funcall fmt "mixc_lor"
  | Lxor -> funcall fmt "mixc_lxor"
  | Lsl -> funcall fmt "mixc_lsl"
  | Lsr -> funcall fmt "mixc_lsr"
  | Asr -> funcall fmt "mixc_asr"
  | Resize_int k -> 
      fprintf fmt "mixc_resize(%a,%d)" pp a k 
  | Print -> 
      skip_when !flag_no_print fmt procall "mixc_print"
  | Print_string ->
      skip_when !flag_no_print fmt procall "mixc_print_string"
  | Print_int ->
      skip_when !flag_no_print fmt procall "mixc_print_int"
  | Print_newline ->
      skip_when !flag_no_print fmt procall "mixc_print_newline"
  | Assert ->
      skip_when !flag_no_assert fmt procall "mixc_skip"
  | String_length ->
      procall fmt "mixc_string_length"
