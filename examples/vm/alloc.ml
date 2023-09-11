
let no_scan_tag : char = 251 ;;
let string_tag : char = 252 ;;
let closure_tag : char = 247 ;;
let infix_tag : char = 249 ;;
let fwd_ptr_tag : char = 248 ;;


let gc_alloc ((heap_top,sz) : (short * short)) : short = 
  heap_top + sz ;;

let size_hd hd =
  (* [as_short] (i.e. resize) is used to forget higher bits (tag) *)
  as_short (hd lsr 2) ;;

let size (ptr:short) : short =
  let hd = ram[ptr-1] in
  size_hd (as_long(ptr_val(hd))) ;;

let tag ptr =
  let hd = ram[ptr-1] in 
  (ptr_val(hd)) lsr 24 ;;

let size_val v = size (ptr_val v) ;;
let tag_val v : short = tag (ptr_val v) ;;


let make_header ((tag,sz) : (char * short)) : long =
  (long_of_char(tag) lsl 24) lor (as_long(sz) lsl 2) ;;

let make_block(heap_top,tag,sz) : (short * value) =
  let sz = if sz = 0 then 1 else sz in 
  let a = gc_alloc (heap_top,sz + 1) in
  ram[a] <- val_int(make_header(tag,sz));
  (heap_top, val_ptr(a+1)) ;;

let make_closure ((heap_top,pc,size) : (short * short * short)) : (short * value) =
  let (next_heap_top, res) = make_block(heap_top,closure_tag,size) in
  set_field(res,0,val_int (as_long(pc)));
  (next_heap_top, res) ;;

