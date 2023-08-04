
let caml_print_int(arg) =
  print_string "======> "; 
  print_int (long_val arg);
  print_newline ();
  val_unit ;;


let caml_fresh_oo_id(arg) = arg ;;
  (* let gensym n = n + 1 in
  val_long (reg gensym last 0) ;;*)

let caml_obj_dup(arg) =
  let sz = size_val(arg) in
  if sz == 0 then arg else
  let tag = tag_val arg in
  let res = make_block(tag,sz) in
  let rec w(i) =
    if i >= sz then () else
    (set_field(res,i,(get_field(arg,i))); w(i+1))
  in 
  w(0);
  res;;