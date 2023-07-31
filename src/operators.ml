
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
  let funcall s = fprintf fmt "%s(%a)" s pp a in
  let procall s = fprintf fmt "%s(%a)" s pp a in
  match op with
  | Add -> funcall @@ "mixc_add"
  | Sub -> funcall @@ "mixc_sub"
  | Mult -> funcall @@ "mixc_mult"
  | Eq -> funcall @@ "mixc_eq"
  | Neq -> funcall @@ "mixc_neq"
  | Lt ->  funcall @@ "mixc_lt"
  | Le -> funcall @@ "mixc_le"
  | Gt -> funcall @@ "mixc_gt"
  | Ge -> funcall @@ "mixc_ge"
  | And -> funcall @@ "mixc_and"
  | Or -> funcall @@ "mixc_or"
  | Xor -> funcall @@ "mixc_xor"
  | Not -> funcall @@ "mixc_not"
  | Abs -> funcall @@ "mixc_abs"
  | Div -> funcall @@  "mixc_div"
  | Mod -> funcall @@ "mixc_mod"
  | Land -> funcall @@ "mixc_land"
  | Lor -> funcall @@ "mixc_lor"
  | Lxor -> funcall @@ "mixc_lxor"
  | Lsl -> funcall @@ "mixc_lsl"
  | Lsr -> funcall @@ "mixc_lsr"
  | Asr -> funcall @@ "mixc_asr"
  | Resize_int k -> fprintf fmt "mixc_resize(%a,%d)" pp a k 
  | Print -> procall @@ "mixc_print"
  | Print_string -> procall @@ "mixc_print_string"
  | Print_int -> procall @@ "mixc_print_int"
  | Print_newline -> procall @@ "mixc_print_newline"
  | Assert -> fprintf fmt "assert(%a(0) = '1')" pp a
  | String_length -> procall @@ "mixc_string_length"
