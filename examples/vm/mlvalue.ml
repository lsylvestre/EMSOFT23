let int_of_bool b = 
  if b then 1 else 0 ;;

let ptr_val v = v lsr 1;; 
let val_ptr v = v lsl 1 ;;
let is_int v = v land 1 ;;
let is_ptr v = int_of_bool (is_int v == 0) ;;
let val_unit = 1 ;;
let long_val v = v lsr 1;;
let val_long n = (n lsl 1) + 1 ;;



let get_field(v,i) =
  ram[ptr_val(v) + i] ;;

let set_field(v,i,w) =
  ram[ptr_val(v) + i] <- w ;;

let gc_alloc sz = 
  let top = heap_top[0] + sz in
  heap_top[0] <- top;
  top ;;

let size_hd hd =
  (hd lsr 2) land (-1) (* 0x003FFFFF *);;

let size ptr =
  let hd = ram[ptr-1] in
  size_hd hd ;;

let tag ptr =
  let hd = ram[ptr-1] in 
  hd lsr 24 ;;

let size_val v = size (ptr_val v) ;;
let tag_val v = tag (ptr_val v) ;;


let no_scan_tag = 251 ;;
let string_tag = 252 ;;
let closure_tag = 247 ;;
let infix_tag = 249 ;;
let fwd_ptr_tag = 248 ;;
