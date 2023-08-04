
let static ram = 0^98303 ;;

let static extra_args = 0^1 ;;

let stack_start = 1000;;
let heap_start = 2000 ;;
let heap_size = 3000 ;;

let stack_size () = 
  heap_start - stack_start ;;

let global_start = 0 ;;

let static env = 0^1;;
let static trap_sp = (-1)^1;;

let static heap_top = 2000^1 ;; (* = heap start *) 



let global_get n =
  ram[global_start + n] ;;

let global_set (n,v) =
  ram[global_start + n] <- v ;;





let rec exit () = exit () ;; (* infinite loop *)

let rec stack_overflow () =
  print "stack overflow"; exit () ;;

let push_stack (v,sp) =
  if (sp - stack_start) >= stack_size () then stack_overflow () else
  ram[sp] <- v;
  sp+1 ;;

let pop_stack (sp) =
  (* assert (sp[0] > 0); *)
  let p = sp-1 in
  let v = ram[p] in 
  (v,p) ;;

let push_stack_implace (sp,v) = (* update [stack] *)
  if (sp - stack_start) >= stack_size () then stack_overflow () else
  ram[sp] <- v ;;


