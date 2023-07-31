
let static ram = 0^20000 ;;
let static data = 0^1000 ;;
let static extra_args = 0^1 ;;

let stack_start = 2000;;
let heap_start = 4000 ;;
let heap_size = 5000 ;;

let stack_size () = 
  heap_start - stack_start ;;

let global_start = 0 ;;

let static data_top = 0^1 ;; (* = heap data_start *) 

let static env = 0^1;;
let static trap_sp = (-1)^1;;

let static heap_top = 4000^1 ;; (* = heap start *) 

(* primitives *)
let int_of_bool b = 
  if b then 1 else 0 ;;



let bnot n = int_of_bool (n = 0) ;; 

let addint (n,m) = n + m ;;
let subint (n,m) = n - m ;;
let mulint (n,m) = n * m ;;
let divint (n,m) = n / m ;; (* todo: prefer a hard-coded division (tail-recursion) *)
let modint (n,m) = n mod m ;; (* todo: prefer a hard-coded modulo (tail-recursion) *)
let andint (n,m) = n land m ;;
let orint (n,m) = n lor m ;;
let xorint (n,m) = n lxor m ;;
let lslint (n,m) = n lsl m ;;
let lsrint (n,m) = n lsr m ;;
let asrint (n,m) = n asr m ;;

let eq (n,m) = int_of_bool (n == m) ;;
let neq (n,m) = int_of_bool (n <> m) ;;
let ltint (n,m) = int_of_bool (n < m) ;;
let leint (n,m) = int_of_bool (n <= m) ;;
let gtint (n,m) = int_of_bool (n > m) ;;
let geint (n,m) = int_of_bool (n >= m) ;;

let compare_imm(n1,n2) =
  if n1 < n2 then -1 else 
  if n1 > n2 then 1 else 0 ;;


(* mlvalues *)

let ptr_val v = v lsr 1;; 
let val_ptr v = v lsl 1 ;;
let is_int v = v land 1 ;;
let is_ptr v = bnot (is_int v) ;;
let val_unit = 1 ;;
let long_val v = v lsr 1;;
let val_long n = (n lsl 1) + 1 ;;
let block_tag b = 0 ;; (* todo *)

