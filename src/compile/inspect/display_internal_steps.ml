
type print_mode = Any | Rename | Front | Encode | Anf | Let_floating | Lambda_lifting
                | Specialize | Inline | Propagation
                | Matching | MiddleEnd

let string_of_print_mode s =
  match s with
  | "none" -> Any
  | "front" -> Front
  | "ren" -> Rename
  | "encode" -> Encode
  | "anf" -> Anf
  | "float" -> Let_floating
  | "lift" -> Lambda_lifting
  | "spec" -> Specialize
  | "inl" -> Inline
  | "prop" -> Propagation
  | "match" -> Matching
  | "middle-end" -> MiddleEnd
  | _ -> failwith "unknown print mode"

let print_mode = ref Any

let set_print_mode s =
  let pm = string_of_print_mode s in
  print_mode := pm

let display_pi a pi =
  if a <> !print_mode then () else
  let open Format in
  fprintf std_formatter "@[<v>{debug mode}===========@,@.%a@]" Ast_pprint.pp_pi pi
