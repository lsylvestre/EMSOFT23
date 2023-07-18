
(**

  Ongoing implementation of the OCaml virtual Machine

  Command to compile:
  ------------------

    ./mixc examples/vm.ml -relax


  Option -relax allows to compile a non-instantaneous program (this example is not reactive !).

*)

let static heap = 0^10000 ;;
let static stack = 0^10000 ;;
let static code = 0^1000 ;;
let static env = 0^100 ;;

let static acc = 0^1 ;;
let static sp = 0^1 ;;
let static pc = 0^1 ;;

let pop_stack () =
  assert (sp[0] > 0);
  let v = stack[sp[0] - 1] in 
  sp[0] <- sp[0] -1;
  v ;;

let pop_stack_ignore n =
  assert (sp[0] >= n);
  sp[0] <- sp[0] - n ;;

let rec exit () = exit () ;;

let stack_overflow () = 
  print "stack overflow"; exit () ;;

let push_stack v =
  if sp[0] >= stack.length - 1 then () (* stack_overflow () *) else (
  stack[sp[0]] <- v; 
  sp[0] <- sp[0] + 1) ;;

let take_argument () =
  pc[0] <- pc[0] + 1;
  code[pc[0]] ;;

let acc_n n = 
  acc[0] <- stack[sp[0] - n - 1] ;;


let push () = 
  push_stack (acc[0]) ;;


let push_acc_n n = 
  push_stack (acc[0]) ;
  acc[0] <- stack[sp[0] - n - 1] ;;

let pop_n n =
  assert (sp[0] >= n);
  sp[0] <- sp[0] - n ;;

let val_long_zero = 0 ;;

let assign n =
  stack[sp[0]-1-n] <- acc[0]; 
  acc[0] <- val_long_zero ;;

let env_acc_n n = 
  acc[0] <- env[n] ;;


let push_env_acc_n n =
  push_stack (acc[0]); 
  acc[0] <- env[n] ;;

(* mlvalues *)
let ptr_val v = v ;; 
let val_ptr v = v ;;
let is_ptr v = true ;; (* todo: returns true if n is pair *)
let val_unit = 0 ;;
let long_val v = v / 2 ;;
let val_long n = n * 2 + 1 ;;

(* primitives *)
let bnot n = if n = 0 then 1 else 0 ;; 
let addint (n,m) = n + m ;;
let subint (n,m) = n - m ;;
let mulint (n,m) = n * m ;;
let divint (n,m) = n / m ;; (* todo: prefer a hard-coded division (tail-recursive) *)
let modint (n,m) = n / m ;; (* todo: prefer a hard-coded modulo (tail-recursive) *)
let andint (n,m) = if (n == 1) && (m == 1) then 1 else 0 ;;
let orint (n,m) = if (n == 1) or (m == 1) then 1 else 0 ;;
let eq (n,m) = if n == m then 1 else 0 ;;
let neq (n,m) = if n <> m then 1 else 0 ;;


let offsetclosure_n n =
  acc[0] <- val_ptr (ptr_val (env[0]) + n) ;;

let pushoffsetclosure_n n =
  push_stack (acc[0]);
  acc[0] <- val_ptr (ptr_val (env[0]) + n) ;;  (* todo: (n * 4) ? *)

let get_field_n n = () ;; (* todo
  assert (is_ptr (acc[0]));
  acc[0] <- heap[acc[n]] ;; *)

let set_field_n n = () ;; (* todo
  assert (is_ptr (acc[0]));
  heap[acc[n]] <- (pop_stack ()); 
  acc[0] <- val_unit ;;*)

let const_n n =
  acc[0] <- n ;; (* Mlvalues.val_long n *)

let pushconst_n n =
  push_stack (acc[0]);
  acc[0] <- n ;; (* Mlvalues.val_long n *)

(*
let appterm nargs n =
  for i = 0 to nargs - 1 do
    Domain.stack.(!Domain.sp - n + i) <- Domain.stack.(!Domain.sp - nargs + i) 
  done;
  pop_stack_ignore (n-nargs);
  pc := Mlvalues.long_val (Block.get_field !Domain.acc 0) - 1;
  Domain.env := !Domain.acc;
  extra_args := !extra_args + nargs - 1
*)


let interp () =
  sp[0] <- 0;
  let rec loop () =
    if pc[0] >= code.length then () else (
      (match code[pc[0]] with
      | 1 (* ACC1 *) -> acc_n 1
      | 2 (* ACC2 *) -> acc_n 2
      | 3 (* ACC3 *) -> acc_n 3
      | 4 (* ACC4 *) -> acc_n 4
      | 5 (* ACC5 *) -> acc_n 5
      | 6 (* ACC6 *) -> acc_n 6
      | 7 (* ACC7 *) -> acc_n 7
      | 8 (* ACC *) -> acc_n (take_argument())
      | 9 (* PUSH *) -> push ()
      | 10 (* PUSHACC0 *) -> push ()
      | 11 (* PUSHACC1 *) -> push_acc_n 1
      | 12 (* PUSHACC2 *) -> push_acc_n 2
      | 13 (* PUSHACC3 *) -> push_acc_n 3
      | 14 (* PUSHACC4 *) -> push_acc_n 4
      | 15 (* PUSHACC5 *) -> push_acc_n 5
      | 16 (* PUSHACC6 *) -> push_acc_n 6
      | 17 (* PUSHACC7 *) -> push_acc_n 7
      | 18 (* PUSHACC *) -> push_acc_n (take_argument ())
      | 19 (* POP *) -> pop_n (take_argument ())
      | 20 (* ASSIGN *) -> assign (take_argument ())
      | 21 (* ENVACC1 *) -> env_acc_n 1
      | 22 (* ENVACC2 *) -> env_acc_n 2
      | 23 (* ENVACC3 *) -> env_acc_n 3
      | 24 (* ENVACC4 *) -> env_acc_n 4
      | 25 (* ENVACC *) -> env_acc_n (take_argument ())
      | 26 (* PUSHENVACC1 *) -> push_env_acc_n 1
      | 27 (* PUSHENVACC2 *) -> push_env_acc_n 2
      | 28 (* PUSHENVACC3 *) -> push_env_acc_n 3
      | 29 (* PUSHENVACC4 *) -> push_env_acc_n 4
      | 30 (* PUSHENVACC *) -> push_env_acc_n (take_argument ())
      
      | 67 (* GETFIELD0 *) -> get_field_n 0
      | 68 (* GETFIELD1 *) -> get_field_n 1
      | 69 (* GETFIELD2 *) -> get_field_n 2
      | 70 (* GETFIELD3 *) -> get_field_n 3
      | 71 (* GETFIELD *) -> get_field_n (take_argument ())

      (* 72 GETFLOATFIELD *)

      | 73 (* SETFIELD0 *) -> set_field_n 0
      | 74 (* SETFIELD1 *) -> set_field_n 1
      | 75 (* SETFIELD2 *) -> set_field_n 2
      | 76 (* SETFIELD3 *) -> set_field_n 3
      | 77 (* SETFIELD *) -> set_field_n (take_argument ())

      (* 78 SETFLOATFIELD *)
      | 88 (* BOOLNOT *) ->
         acc[0] <- val_long (bnot (long_val (acc[0])))
      | 99  (* CONST0 *) -> const_n 0
      | 100 (* CONST1 *) -> const_n 1
      | 101 (* CONST2 *) -> const_n 2
      | 102 (* CONST3 *) -> const_n 3
      | 103 (* CONSTINT *) -> const_n (take_argument ())
      | 104 (* PUSHCONST0 *) -> pushconst_n 0
      | 105 (* PUSHCONST1 *) -> pushconst_n 1
      | 106 (* PUSHCONST2 *) -> pushconst_n 2
      | 107 (* PUSHCONST3 *) -> pushconst_n 3
      | 108 (* PUSHCONSTINT *) -> pushconst_n (take_argument ())
      | 110 (* ADDINT *) ->
         acc[0] <- val_long (addint (long_val (acc[0]), long_val (pop_stack ())))
      | 111 (* SUBINT *) ->
         acc[0] <- val_long (subint (long_val (acc[0]), long_val (pop_stack ())))
      | 112 (* MULINT *) ->
         acc[0] <- val_long (mulint (long_val (acc[0]), long_val (pop_stack ())))
      | 113 (* DIVINT *) ->
         acc[0] <- val_long (divint (long_val (acc[0]), long_val (pop_stack ())))
      | 114 (* DIVINT *) ->
         acc[0] <- val_long (modint (long_val (acc[0]), long_val (pop_stack ())))
      | 115 (* ANDINT *) ->
         acc[0] <- val_long (andint (long_val (acc[0]), long_val (pop_stack ())))
      | 116 (* ORINT *) ->
         acc[0] <- val_long (orint (long_val (acc[0]), long_val (pop_stack ())))
      
      | 121 (* EQ *) ->
         acc[0] <- val_long (eq (long_val (acc[0]), long_val (pop_stack ())))
      | 122 (* NEQ *) ->
         acc[0] <- val_long (neq (long_val (acc[0]), long_val (pop_stack ())))

      | x -> () 
    end);
    loop ()
    )
  in loop () ;;

let main () = interp () ;;
(*
let pop_stack () = 
  push_stack !Domain.acc



let rec loop (s,i) =
  if i < a.length
  then loop ((a[i] + s), i+1)
  else s ;;

let main () =
  exec loop (0,0) default 0 ;;
*)