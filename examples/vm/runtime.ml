
(**

  Ongoing implementation of the OCaml Virtual Machine

  Command to compile:
  ------------------

    ./mixc examples/vm/bcprepare/code.ml examples/vm/vm.ml -relax


  Option -relax allows to compile a non-instantaneous program (this example is not reactive !).

*)
let pop_stack_implace (sp_minus_1) =
  (* assert (sp[0] > 0); (see bug 3) *)
  let v = ram[sp_minus_1] in 
  v ;;

let rec exit () = exit () ;; (* infinite loop *)

let rec stack_overflow () =
  print "stack overflow"; exit () ;;

let ppush_stack (v,sp) =
  if (sp - stack_start) >= stack_size () then stack_overflow () else
  ram[sp] <- v;
  sp+1 ;;

let pop_stack (sp) =
  (* assert (sp[0] > 0); (see bug 3) *)
  let p = sp-1 in
  let v = ram[p] in 
  (v,p) ;;

let push_stack_implace (sp,v) = (* update [stack] *)
  if (sp - stack_start) >= stack_size () then stack_overflow () else
  ram[sp] <- v ;;





(*** unsigned comparison (<)                  ***)
(*** n1 < 0 && n2 >= 0 => (ultint n1 n2) ~> 0 ***)
let ultint (n1,n2) =
  if n1 < 0 then (if n2 < 0 then gtint(n1,n2) else 0)
  else (if n2 < 0 then 0 else ltint(n1,n2)) ;;


(*** unsigned comparison (>=)                 ***)
(*** n1 < 0 && n2 >= 0 => (ugeint n1 n2) ~> 1 ***)
let ugeint (n1,n2) =
  if n1 < 0 then (if n2 < 0 then leint(n1,n2) else 1)  
  else (if n2 < 0 then 1 else geint(n1,n2)) ;;

(* ********** block ********** *)

let get_field(v,i) =
  ram[ptr_val(v) + i] ;;

let set_field(v,i,w) =
  ram[ptr_val(v) + i] <- w ;;

let gc_alloc sz = 
  let top = heap_top[0] + sz in
  heap_top[0] <- top;
  top ;;

let size_hd hd =
  (hd lsr 2) land 4194303 (* 0x003FFFFF *);;

let size ptr =
  let hd = ram[ptr-1] in
  size_hd hd ;;

let tag ptr =
  let hd = ram[ptr-1] in 
  hd lsr 24 ;;

let size_val v = size (ptr_val v) ;;
let tag_val v = tag (ptr_val v) ;;

let make_header(tag,sz) =
  let n = (tag lsl 24) lor (sz lsl 2) in
  n 
;;

let make_block(tag,sz) =
  let sz = if sz = 0 then 1 else sz in
  let a = gc_alloc (sz + 1) in
  ram[a] <- make_header(tag,sz);
  let v = val_ptr(a+1) in
   v
   ;; (* not checked yet *)


let no_scan_tag = 251 ;;
let string_tag = 252 ;;
let closure_tag = 247 ;;
let infix_tag = 249 ;;
let fwd_ptr_tag = 248 ;;

let make_closure(pc,size) =
  let res = make_block(closure_tag,size) in
  set_field(res,0,val_long pc);
  res ;;


(* *********** debug ********** *)


let print_block(ptr) =
  let n = size(ptr) in
  print_string "block: [";
  let rec w(i) =
    if i > n then () else
    (print_int (ram[ptr+i-1]); print_string "|"; w(i+1))
  in
  w(0);
  print_string "]";
  print_newline () ;;



let print_stack(sp) =
  print_string "stack: [";
  let rec w(i) =
    if i >= sp then () else
    (print_int (ram[i]); print_string "|"; w(i+1))
  in
  w(stack_start);
  print_string "]";
  print_newline () ;;


let global_get n =
  ram[global_start + n] ;;

let global_set (n,v) =
  ram[global_start + n] <- v ;;





(* ******** prims ********** *)

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
