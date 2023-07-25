
(**

  Ongoing implementation of the OCaml Virtual Machine

  Command to compile:
  ------------------

    ./mixc examples/vm.ml -relax


  Option -relax allows to compile a non-instantaneous program (this example is not reactive !).

*)

let static ram = 0^20000 ;;

let static code = 0^1000 ;;
let static data = 0^1000 ;;
let static extra_args = 0^1 ;;

let heap_start = 0 ;;
let stack_start = 2000;;
let stack_size = 1000;;
let global_start = 10000 ;;
let static env = 0^1;;
let static from_space = 5000^1;;

let static heap_top = 0^1 ;; 

let global_get n =
  ram[global_start + n] ;;

let global_set (n,v) =
  ram[global_start + n] <- v ;;

let pop_stack (sp_minus_1) =
  (* assert (sp[0] > 0); (see bug 3) *)
  let v = ram[sp_minus_1] in 
  v ;;

let rec exit () = exit () ;;

let rec stack_overflow () = 
  print "stack overflow"; exit () ;;

let push_stack (sp,v) = (* update [stack] *)
  if (sp - stack_start) >= stack_size then stack_overflow () else
  ram[sp] <- v ;;


let bnot n = if n = 0 then 1 else 0 ;; 

(* mlvalues *)
let ptr_val v = v ;; 
let val_ptr v = v ;;
let is_int v = v mod 2 ;;
let is_ptr v = bnot (is_int v) ;;
let val_unit = 0 ;;
let long_val v = v lsr 1;;
let val_long n = (n lsl 1) + 1 ;;
let block_tag b = 0 ;; (* todo *)


(* primitives *)

let addint (n,m) = n - m ;;
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

let int_of_bool b = if b then 1 else 0 ;;
let eq (n,m) = int_of_bool (n == m) ;;
let neq (n,m) = int_of_bool (n <> m) ;;
let ltint (n,m) = int_of_bool (n < m) ;;
let leint (n,m) = int_of_bool (n <= m) ;;
let gtint (n,m) = int_of_bool (n > m) ;;

let geint (n,m) = int_of_bool (n >= m) ;;

let compare_imm(n1,n2) =
  if n1 < n2 then -1 else 
  if n1 > n2 then 1 else 0 ;;

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
  ram[ptr_val(v) + i + 1] ;;

let set_field(v,i,w) =
  ram[ptr_val(v) + i + 1] <- w ;;

let gc_alloc sz = 
  let top = heap_top[0] + sz in
  heap_top[0] <- top;
  top ;;

let make_header(tag,sz) =
  val_long (tag + 256 * sz) ;;

let make_block(tag,sz) =
  let sz = if sz = 0 then 1 else sz in
  let a = gc_alloc (sz + 1) in
  ram[a] <- make_header(tag,sz);
  val_ptr(a) ;; (* not checked yet *)

(* ********************************************** *)

let interp () =

(* example of code: *)
  code[0] <- 101;
  code[1] <- 102;
  code[2] <- 103;
  code[3] <- 42;
  code[4] <- 100; (*
  code[1] <- 127;
  code[2] <- 5;
  code[3] <- 101;*)
  (*
  code[3] <- 1;
  code[4] <- 103;
  code[5] <- 2;
  code[6] <- 103;
  code[7] <- 42;
  code[8] <- 84;
  code[9] <- 2;
   code[2] <- 9; *)

  let rec loop (pc,acc,sp) =
    print (pc,long_val acc);

    (*  ************* precomputed values  ************* *)

    let pc_plus_1 = pc + 1 in
    let pc_plus_2 = pc + 2 in
    let pc_plus_3 = pc + 3 in
    let sp_plus_1 = sp + 1 in
    let sp_minus_1 = sp - 1 in

    (*  ************* instructions ************* *)

    let acc_n (next_pc,n) = 
      (next_pc, ram[sp_minus_1 - n], sp) in

    let push (next_pc,acc) =
      push_stack(sp,acc);
      (next_pc, acc, sp_plus_1) in

    let push_acc_n (next_pc,n) =
      push_stack(sp,acc);
      (next_pc,ram[sp_minus_1 - n],sp_plus_1) in

    let env_acc_n (next_pc,n) =
      (next_pc,ram[env[0]+n],sp) in

    let push_env_acc_n (next_pc,n) =
      (next_pc,ram[env[0]+n],sp_plus_1) in

    let offsetclosure_n (next_pc,n) =
      let v = val_ptr (ptr_val (ram[env[0]]) + n) in
      (next_pc, v, sp) in

    let pushoffsetclosure_n (next_pc,n) =
      push_stack (sp,acc);
      let v = val_ptr (ptr_val (ram[env[0]]) + n) in
      (next_pc,v,sp_plus_1) in

    let get_field_n (next_pc,n) =
      assert (is_ptr (acc) == 0);
      (next_pc,ram[acc+n],sp) in

    let set_field_n (next_pc,n) =
      assert (is_ptr (acc) == 0);
      ram[acc+n] <- (pop_stack(sp_minus_1)); 
      (next_pc,val_unit,sp_minus_1) in

    let const_n (next_pc,n) =
      (next_pc,val_long n,sp) in

    let pushconst_n (next_pc,n) =
      push_stack (sp,acc);
      (next_pc,val_long n,sp_plus_1) in

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

    let binop (op) =
      let v = val_long (op (long_val (acc), long_val (pop_stack(sp_minus_1)))) in
      (pc_plus_1,v,sp_minus_1) in

    let branch_when_apply_acc_code_test (test) =
      let v = code[pc_plus_1] in
      if test(compare_imm(v,acc),0)
      then (let ofs = code[pc_plus_2] in (ofs-1,acc,sp))
      else (pc_plus_3,acc,sp) in

    let apply_acc_pop (f) =
      let v = val_long(f(long_val acc,long_val(pop_stack(sp_minus_1)))) in
      (pc_plus_1,v,sp_minus_1) in

    let branch_when_apply_acc_code (compare) =
      let v = code[pc_plus_1] in
      if compare(v,acc) == 1
      then (let ofs = code[pc_plus_2] in (ofs-1,acc,sp))
      else (pc_plus_3,acc,sp) in

    (* ************************************)

    let on_argument (f) =
      let v = code[pc_plus_1] in
      f v
    in
    let on_arguments2 (f) =
      let v1 = code[pc_plus_1] in
      let v2 = code[pc_plus_2] in
      f (v1,v2)
    in
    
    if pc >= code.length then pc else
    loop (match code[pc] with       
          | 1 (* ACC1 *) -> acc_n(pc_plus_1, 1)
          | 2 (* ACC2 *) -> acc_n(pc_plus_1, 1)
          | 3 (* ACC3 *) -> acc_n(pc_plus_1, 1)
          | 4 (* ACC4 *) -> acc_n(pc_plus_1, 1)
          | 5 (* ACC5 *) -> acc_n(pc_plus_1, 1)
          | 6 (* ACC6 *) -> acc_n(pc_plus_1, 1)
          | 7 (* ACC7 *) -> acc_n(pc_plus_1, 1)
          | 8 (* ACC *) -> let f n =
                             acc_n(pc_plus_1, n)
                           in
                           on_argument f
          | 9 (* PUSH *) -> push(pc_plus_1,acc)
          | 10 (* PUSHACC0 *) -> push(pc_plus_1,acc)
          | 11 (* PUSHACC1 *) -> push_acc_n(pc_plus_1, 1)
          | 12 (* PUSHACC2 *) -> push_acc_n(pc_plus_1, 2)
          | 13 (* PUSHACC3 *) -> push_acc_n(pc_plus_1, 3)
          | 14 (* PUSHACC4 *) -> push_acc_n(pc_plus_1, 4)
          | 15 (* PUSHACC5 *) -> push_acc_n(pc_plus_1, 5)
          | 16 (* PUSHACC6 *) -> push_acc_n(pc_plus_1, 6)
          | 17 (* PUSHACC7 *) -> push_acc_n(pc_plus_1, 7)
          | 18 (* PUSHACC *) -> let f n = 
                                  push_acc_n (pc_plus_2, n)
                                in
                                on_argument f
          | 19 (* POP *) -> let pop_n n =
                               assert (sp - stack_start >= n);
                               sp - n
                            in
                           (pc_plus_2, acc, on_argument pop_n)
          | 20 (* ASSIGN *) -> let assign (n) =
                                  ram[sp_minus_1-n] <- acc
                               in on_argument assign;
                               (pc_plus_2,val_long 0,sp)
          | 21 (* ENVACC1 *) -> env_acc_n(pc_plus_1, 1)
          | 22 (* ENVACC2 *) -> env_acc_n(pc_plus_1, 2)
          | 23 (* ENVACC3 *) -> env_acc_n(pc_plus_1, 3)
          | 24 (* ENVACC4 *) -> env_acc_n(pc_plus_1, 4)
          | 25 (* ENVACC *) -> let f n =
                                 env_acc_n(pc_plus_2, 1)
                               in
                               on_argument f
          | 26 (* PUSHENVACC1 *) -> push_env_acc_n(pc_plus_1, 1)
          | 27 (* PUSHENVACC2 *) -> push_env_acc_n(pc_plus_1, 2)
          | 28 (* PUSHENVACC3 *) -> push_env_acc_n(pc_plus_1, 3)
          | 29 (* PUSHENVACC4 *) -> push_env_acc_n(pc_plus_1, 4)
          | 30 (* PUSHENVACC *) -> let f n =
                                     push_env_acc_n(pc_plus_2, n)
                                   in
                                   on_argument f
         
          | 31 (* PUSH-RETADDR *) -> let ofs = code[pc_plus_1] in
                                     push_stack(val_long (extra_args[0]),sp);
                                     push_stack(val_long (env[0]),sp_plus_1);
                                     push_stack(val_long ofs,sp_plus_1+1);
                                     (pc_plus_2,acc,sp_plus_1+1)
          | 32 (* APPLY *) -> let args = code[pc_plus_1] in
                              extra_args[0] <- args - 1;
                              env[0] <- acc;
                              let next_pc = long_val (get_field(acc,0)) in
                              (next_pc,acc,sp)





          | 45 (* OFFSETCLOSUREM2 *) -> offsetclosure_n(pc_plus_1, -2)
          | 46 (* OFFSETCLOSURE0 *) -> offsetclosure_n(pc_plus_1, 0)
          | 47 (* OFFSETCLOSURE2 *) -> offsetclosure_n(pc_plus_1, 2)
          | 48 (* OFFSETCLOSURE *) -> let f n =
                                         offsetclosure_n(pc_plus_2, n)
                                      in
                                      on_argument f

          | 49 (* PUSHOFFSETCLOSUREM2 *) -> pushoffsetclosure_n (pc_plus_1, -2)
          | 50 (* PUSHOFFSETCLOSURE0 *) -> pushoffsetclosure_n (pc_plus_1, 0)
          | 51 (* PUSHOFFSETCLOSURE2 *) -> pushoffsetclosure_n (pc_plus_1, 2)
          | 52 (* PUSHOFFSETCLOSURE *) -> let f n = 
                                              pushoffsetclosure_n (pc_plus_2, n)
                                          in
                                          on_argument f
          | 53 (* GETGLOBAL *) -> (pc_plus_2, on_argument global_get,sp) 
          | 54 (* PUSHGETGLOBAL *) -> push_stack(acc,sp);
                                      (pc_plus_2, on_argument global_get,sp_plus_1)
          | 55 (* GETGLOBALFIELD *) -> let f (n,p) = get_field(global_get n,p) in
                                       (pc_plus_2+1,on_arguments2 f,sp)
          | 56 (* PUSHGETGLOBALFIELD *) -> 
                push_stack(sp,acc);
                let f (n,p) = get_field(global_get n,p) in
                (pc_plus_2+1,on_arguments2 f,sp_plus_1)
          | 57 (* SETGLOBAL *) -> let f n =
                                    global_set(n,acc) 
                                  in on_argument f;
                                  (pc_plus_2,acc,sp)
          | 58 (* ATOM0 *) -> (pc_plus_1, make_block(0,0), sp)
          | 59 (* ATOM *) -> let f tag = make_block(tag,0) in
                             (pc_plus_2,on_argument f,sp)
          | 60 (* PUSHATOM0 *) -> push_stack(sp,acc);
                                  (pc_plus_1, make_block(0,0), sp_plus_1)
          | 61 (* PUSHATOM *) -> push_stack(sp,acc);
                                 let f tag = make_block(tag,0) in
                                 (pc_plus_2,on_argument f,sp_plus_1)
          | 62 (* MAKEBLOCK *) -> let sz = code[pc_plus_1] in
                                  let tag = code[pc_plus_2] in 
                                  let blk = make_block(tag,sz) in
                                  set_field(blk,0,acc);
                                  let rec fill(i) =
                                     if i >= sz then () else
                                     (set_field(blk,i,pop_stack(sp-i)); fill(i+1))
                                  in fill(1);
                                  (pc_plus_2+1,blk,sp_minus_1-sz) (* sp_minus_1-sz: ok ? *) 
          | 63 (* MAKEBLOCK1 *) -> let tag = code[pc_plus_2] in 
                                   let blk = make_block(tag,1) in
                                   set_field(blk,0,acc);
                                   (pc_plus_2,blk,sp)
          | 64 (* MAKEBLOCK2 *) -> let tag = code[pc_plus_2] in 
                                   let blk = make_block(tag,2) in
                                   set_field(blk,0,acc);
                                   set_field(blk,1,pop_stack(sp_minus_1));
                                   (pc_plus_2,blk,sp_minus_1)
          | 65 (* MAKEBLOCK3 *) -> let tag = code[pc_plus_2] in 
                                   let blk = make_block(tag,3) in
                                   set_field(blk,0,acc);
                                   set_field(blk,1,pop_stack(sp_minus_1));
                                   let sp_minus_2 = sp_minus_1-1 in
                                   set_field(blk,2,pop_stack(sp_minus_2));
                                   (pc_plus_2,blk,sp_minus_2)
         (* 66 MAKEFLOATBLOCK *)
          | 67 (* GETFIELD0 *) -> get_field_n(pc_plus_1, 0)
          | 68 (* GETFIELD1 *) -> get_field_n(pc_plus_1, 1)
          | 69 (* GETFIELD2 *) -> get_field_n(pc_plus_1, 2)
          | 70 (* GETFIELD3 *) -> get_field_n(pc_plus_1, 3)
          | 71 (* GETFIELD *) -> let f n =
                                   get_field_n (pc_plus_2, n)
                                 in
                                 on_argument f

          (* 72 GETFLOATFIELD *)

          | 73 (* SETFIELD0 *) -> set_field_n(pc_plus_1, 0)
          | 74 (* SETFIELD1 *) -> set_field_n(pc_plus_1, 1)
          | 75 (* SETFIELD2 *) -> set_field_n(pc_plus_1, 2)
          | 76 (* SETFIELD3 *) -> set_field_n(pc_plus_1, 3)
          | 77 (* SETFIELD *) -> let f n =
                                   set_field_n(pc_plus_2, n)
                                 in
                                 on_argument f

          (* 78 SETFLOATFIELD *)

          | 84 (* BRANCH *) -> (code[pc_plus_1], acc, sp)
          | 85 (* BRANCHIF *) -> let next_pc = if (is_ptr(acc) == 0)
                                                 && (long_val(acc) <> 0) 
                                                then code[pc_plus_1] (* caution with address zero *)
                                                else pc_plus_2
                                 in (next_pc, acc, sp)
          | 86 (* BRANCHIFNOT *) -> let next_pc = if (is_ptr(acc) == 0) 
                                                 && (long_val(acc) == 0) 
                                                then code[pc_plus_1] (* caution with address zero *)
                                                else pc_plus_2
                                    in (next_pc, acc, sp)
          (*| 87 (* SWITCH *) ->   
             let n = take_argument () in 
             (if is_ptr (acc[0]) 
             then pc[0] <- (let idx = block_tag (ptr_val (acc[0])) in
                            let ofs = 2 * (n + idx) + 1 in
                            ofs - 1)
             else pc[0] <- long_val (acc[0])); 
             pause () (* wait pc[0] update *)*)
          | 88 (* BOOLNOT *) ->
             (pc_plus_1,val_long (bnot (long_val (acc))), sp)
          (* 89 PUSHTRAP *)
          (* 91 POPTRAP *)
          (* 91 RAISE *)
          (* 92 CHECK-SIGNALS *)
          (* 93 C-CALL1 *)
          (* 94 C-CALL2 *)
          (* 95 C-CALL3 *)
          (* 96 C-CALL4 *)
          (* 97 C-CALL5 *)
          (* 98 C-CALLN *)
          | 99  (* CONST0 *) -> const_n(pc_plus_1, 0)
          | 100 (* CONST1 *) -> const_n(pc_plus_1, 1)
          | 101 (* CONST2 *) -> const_n(pc_plus_1, 2)
          | 102 (* CONST3 *) -> const_n(pc_plus_1, 3)
          | 103 (* CONSTINT *) -> let f n =
                                    const_n(pc_plus_2, n)
                                  in
                                  on_argument f
          | 104 (* PUSHCONST0 *) -> pushconst_n(pc_plus_1, 0)
          | 105 (* PUSHCONST1 *) -> pushconst_n(pc_plus_1, 1)
          | 106 (* PUSHCONST2 *) -> pushconst_n(pc_plus_1, 2)
          | 107 (* PUSHCONST3 *) -> pushconst_n(pc_plus_1, 3)
          | 108 (* PUSHCONSTINT *) -> let f n =
                                        pushconst_n(pc_plus_2, n)
                                      in
                                      on_argument f
          | 110 (* ADDINT *) -> binop(addint)
          | 111 (* SUBINT *) -> binop(subint)
          | 112 (* MULINT *) -> binop(mulint)
          | 113 (* DIVINT *) -> binop(divint)
          | 114 (* MODINT *) -> binop(modint)
          | 115 (* ANDINT *) -> binop(andint)
          | 116 (* ORINT *) -> binop(orint)
          | 117 (* XORINT *) -> binop(xorint)
          | 118 (* LSLINT *) -> binop(lslint)
          | 119 (* LSRINT *) -> binop(lsrint)
          | 120 (* ASRINT *) -> binop(asrint)
          | 121 (* EQ *) -> binop(eq)
          | 122 (* NEQ *) -> binop(neq)
          | 123 (* LTINT *) -> binop(ltint)
          | 124 (* LEINT *) -> binop(leint)
          | 125 (* GTINT *) -> binop(gtint)
          | 126 (* GEINT *) -> binop(geint)
          | 127 (* OFFSETINT *) -> 
             let f(ofs) = val_long(addint(long_val acc, ofs)) in
             (pc_plus_2, on_argument f, sp) 
(*
      | 128 (* OFFSETREF *) -> let ofs = take_argument code in
                               let old = Block.get_field !Domain.acc 0 in
                               Block.set_field !Domain.acc 0
                                 (Mlvalues.val_long @@
                                    Prims.addint (Mlvalues.long_val old) ofs);
                               Domain.acc := Block.unit*)
          | 129 (* ISINT *) -> (pc_plus_1, val_long (is_int(acc)), sp)
    
      (*| 130 (* GETMETHOD *) -> (* todo *)
         let x = pop_stack () in 
         let y = Block.get_field x 0 in
         Domain.acc := Block.get_field y
                         (Mlvalues.long_val @@ !Domain.acc)*)
          | 131 (* BEQ *) -> branch_when_apply_acc_code_test((==))
          | 132 (* BNEQ *) -> branch_when_apply_acc_code_test((<>))
          | 133 (* BLTINT *) -> branch_when_apply_acc_code_test((<))
          | 134 (* BLEINT *) -> branch_when_apply_acc_code_test((<=))
          | 135 (* BGTINT *) -> branch_when_apply_acc_code_test((>))
          | 136 (* BGEINT *) -> branch_when_apply_acc_code_test((>=))
          | 137 (* ULTINT *) -> apply_acc_pop(ultint)
          | 138 (* UGEINT *) -> apply_acc_pop(ugeint)
          | 139 (* BULTINT *) -> branch_when_apply_acc_code(ultint)
          | 140 (* BUGEINT *) -> branch_when_apply_acc_code(ugeint)

          | 143 (* STOP *) -> (code.length, acc, sp)
          | _ -> (pc,acc, sp)
          end)
  in loop (0,0,stack_start) ;;

let main () = interp () ;;
