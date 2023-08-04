
(* ********************************************** *)

let interp () =
  let rec loop (pc,acc,sp) =
    
    print_string "pc:";   print_int pc;
    print_string "|acc:"; print_int acc;
    print_string "|sp:";  print_int sp;
    (* print_string "stack:"; print_stack(sp); *)
    print_newline ();

    (*  ************* precomputed values  ************* *)

    let pc_plus_1 = pc + 1 in
    let pc_plus_2 = pc + 2 in
    let pc_plus_3 = pc + 3 in
    let sp_plus_1 = sp + 1 in
    let sp_plus_2 = sp + 2 in
    let sp_minus_1 = sp - 1 in
    let sp_minus_2 = sp - 2 in

    (*  ************* instructions ************* *)

    let on_argument (f) =
      let v = code[pc_plus_1] in f v in

    let on_arguments2 (f) =
      let v1 = code[pc_plus_1] in
      let v2 = code[pc_plus_2] in f (v1,v2) in

    let on_arguments3 (f) =
      let v1 = code[pc_plus_1] in
      let v2 = code[pc_plus_2] in
      let v3 = code[pc_plus_3] in f (v1,v2,v3) in

    let acc_n (next_pc,n) =
      (next_pc, ram[sp_minus_1 - n], sp) in

    let push() =
      let sp = push_stack(acc,sp) in
      (pc_plus_1, acc, sp) in

    let push_acc_n (next_pc,n) =
      let sp_minus_1 = push_stack(acc,sp) in
      (next_pc,ram[sp - n],sp_minus_1) in

    let env_acc_n (next_pc,n) =
      (next_pc,get_field(env[0],n),sp) in

    let push_env_acc_n (next_pc,n) =
      (next_pc,get_field(env[0],n),sp_plus_1) in

    let offsetclosure_n (next_pc,n) =
      let v = val_ptr (ptr_val (env[0]) + n) in
      (next_pc, v, sp) in

    let pushoffsetclosure_n (next_pc,n) =
      let sp = push_stack(acc,sp) in
      let v =  val_ptr (ptr_val (env[0]) + n) in
      (next_pc,v,sp) in

    let get_field_n (next_pc,n) =
      assert (is_ptr (acc) == 1);
      let v = ram[ptr_val(acc)+n] in
      (next_pc,v,sp) in

    let set_field_n (next_pc,n) =
      assert (is_ptr (acc) == 2);
      ram[ptr_val(acc)+n] <- (pop_stack_implace(sp_minus_1)); 
      (next_pc,val_unit,sp_minus_1) in

    let const_n (next_pc,n) =
      (next_pc,val_long n,sp) in

    let pushconst_n (next_pc,n) =
      let sp = push_stack(acc,sp) in
      (next_pc,val_long n,sp) in

    let binop (op) =
      let v = val_long (op (long_val (acc), long_val (pop_stack_implace(sp_minus_1)))) in
      (pc_plus_1,v,sp_minus_1) in

    let compbranch (test) =
      on_arguments2 (fun (v,ofs) ->
        if test(v,long_val acc)
        then (pc_plus_2+ofs,acc,sp)
        else (pc_plus_3,acc,sp)) in

    let apply_acc_pop (f) =
      let v = val_long(f(long_val acc,long_val(pop_stack_implace(sp_minus_1)))) in
      (pc_plus_1,v,sp_minus_1) in

    let branch_when_apply_acc_code (compare) =
      let v = code[pc_plus_1] in
      if compare(v,acc) == 1
      then (let ofs = code[pc_plus_2] in (pc_plus_2+ofs,acc,sp))
      else (pc_plus_3,acc,sp) in

    (* ************************************)
    
    if pc >= code.length then pc else
    loop (match resize_int<8> (code[pc]) with       
          | 0 (* ACC0 *) -> acc_n(pc_plus_1, 0)
          | 1 (* ACC1 *) -> acc_n(pc_plus_1, 1)
          | 2 (* ACC2 *) -> acc_n(pc_plus_1, 2)
          | 3 (* ACC3 *) -> acc_n(pc_plus_1, 3)
          | 4 (* ACC4 *) -> acc_n(pc_plus_1, 4)
          | 5 (* ACC5 *) -> acc_n(pc_plus_1, 5)
          | 6 (* ACC6 *) -> acc_n(pc_plus_1, 6)
          | 7 (* ACC7 *) -> acc_n(pc_plus_1, 7)
          | 8 (* ACC *) -> on_argument (fun n -> acc_n(pc_plus_1, n))
          | 9 (* PUSH *) -> push()
          | 10 (* PUSHACC0 *) -> push()
          | 11 (* PUSHACC1 *) -> push_acc_n(pc_plus_1, 1)
          | 12 (* PUSHACC2 *) -> push_acc_n(pc_plus_1, 2)
          | 13 (* PUSHACC3 *) -> push_acc_n(pc_plus_1, 3)
          | 14 (* PUSHACC4 *) -> push_acc_n(pc_plus_1, 4)
          | 15 (* PUSHACC5 *) -> push_acc_n(pc_plus_1, 5)
          | 16 (* PUSHACC6 *) -> push_acc_n(pc_plus_1, 6)
          | 17 (* PUSHACC7 *) -> push_acc_n(pc_plus_1, 7)
          | 18 (* PUSHACC *) ->  on_argument (fun n -> push_acc_n (pc_plus_2, n))
          | 19 (* POP *) -> on_argument (fun n -> (pc_plus_2, acc, sp - n))
          | 20 (* ASSIGN *) -> on_argument (fun n -> (* not yet checked *)
                                  ram[sp_minus_1-n] <- acc;
                                  (pc_plus_2,val_unit,sp))
          | 21 (* ENVACC1 *) -> env_acc_n(pc_plus_1, 1)
          | 22 (* ENVACC2 *) -> env_acc_n(pc_plus_1, 2)
          | 23 (* ENVACC3 *) -> env_acc_n(pc_plus_1, 3)
          | 24 (* ENVACC4 *) -> env_acc_n(pc_plus_1, 4)
          | 25 (* ENVACC *) -> on_argument (fun n -> env_acc_n(pc_plus_2, 1))
          | 26 (* PUSHENVACC1 *) -> push_env_acc_n(pc_plus_1, 1)
          | 27 (* PUSHENVACC2 *) -> push_env_acc_n(pc_plus_1, 2)
          | 28 (* PUSHENVACC3 *) -> push_env_acc_n(pc_plus_1, 3)
          | 29 (* PUSHENVACC4 *) -> push_env_acc_n(pc_plus_1, 4)
          | 30 (* PUSHENVACC *) -> on_argument (fun n -> push_env_acc_n(pc_plus_2, n))
          | 31 (* PUSH-RETADDR *) -> on_argument (fun ofs ->
                                       let sp = push_stack(val_long (extra_args[0]),sp) in
                                       let sp = push_stack(val_long (env[0]),sp) in
                                       let sp = push_stack(val_long (pc_plus_1+ofs),sp) in
                                       (pc_plus_2,acc,sp))
          | 32 (* APPLY *) -> on_argument (fun args ->
                                extra_args[0] <- args - 1;
                                let next_pc = long_val (get_field(acc,0)) in
                                env[0] <- acc;
                                (next_pc,acc,sp))
          | 33 (* APPLY1 *) -> assert (is_ptr acc == 1);
                               let (arg,sp) = pop_stack(sp) in
                               let sp = push_stack(val_long (extra_args[0]),sp) in
                               let sp = push_stack(env[0],sp) in
                               let sp = push_stack(val_long pc_plus_1,sp) in
                               let sp = push_stack(arg,sp) in
                               let next_pc = long_val (get_field(acc,0)) in
                               env[0] <- acc;
                               extra_args[0] <- 0;
                               (next_pc,acc,sp)
          | 34 (* APPLY2 *) -> let (arg1,sp) = pop_stack(sp) in
                               let (arg2,sp) = pop_stack(sp) in
                               let sp = push_stack(val_long (extra_args[0]),sp) in
                               let sp = push_stack (env[0],sp) in
                               let sp = push_stack(val_long pc_plus_1,sp) in
                               let sp = push_stack(arg2,sp) in
                               let sp = push_stack(arg1,sp) in
                               let next_pc = long_val (get_field(acc,0)) in
                               env[0] <- acc;
                               extra_args[0] <- 1;
                               (next_pc,acc,sp)                 

          | 35 (* APPLY3 *) -> let (arg1,sp) = pop_stack(sp) in
                               let (arg2,sp) = pop_stack(sp) in
                               let (arg3,sp) = pop_stack(sp) in
                               let sp = push_stack(val_long (extra_args[0]),sp) in
                               let sp = push_stack (env[0],sp_minus_2) in
                               let sp = push_stack(val_long pc_plus_1,sp_minus_1) in
                               let sp = push_stack(arg3,sp) in
                               let sp = push_stack(arg2,sp_plus_1) in
                               let sp = push_stack(arg1,sp_plus_2) in
                               let next_pc = long_val (get_field(acc,0)) in
                               env[0] <- acc;
                               extra_args[0] <- 2;
                               (next_pc,acc,sp)          

           | 36 (* APPTERM *) -> on_arguments2 (fun (n,s) -> (* not yet checked *)
                                  let rec w i =
                                    if i > n then () else
                                    ram[sp-n-s-i] <- ram[sp-i]; w(i+1)
                                  in w(1);
                                  let next_sp = sp - n - s in
                                  let next_pc = long_val (get_field(acc,0)) in
                                  env[0] <- acc;
                                  extra_args[0] <- extra_args[0] + n - 1;
                                  (next_pc,acc,next_sp))
          | 37 (* APPTERM1 *) -> on_argument (fun n ->
                                    let (arg,sp) = pop_stack(sp) in
                                    let sp = sp - n + 1 in
                                    let sp = push_stack(arg,sp) in
                                    env[0] <- acc;
                                    let next_pc = long_val (get_field(acc,0)) in
                                    (next_pc,acc,sp))
          | 38 (* APPTERM2 *) -> on_argument (fun n ->
                                    let (arg1,sp) = pop_stack(sp) in
                                    let (arg2,sp) = pop_stack(sp) in
                                    let sp = sp - n + 2 in
                                    let sp = push_stack(arg2,sp) in
                                    let sp = push_stack(arg1,sp) in
                                    env[0] <- acc;
                                    let next_pc = long_val (get_field(acc,0)) in
                                    extra_args[0] <- extra_args[0] + 1;
                                    (next_pc,acc,sp))
          | 39 (* APPTERM3 *) -> on_argument (fun n ->
                                    let (arg1,sp) = pop_stack(sp) in
                                    let (arg2,sp) = pop_stack(sp) in
                                    let (arg3,sp) = pop_stack(sp) in
                                    let sp = sp - n + 3 in
                                    let sp = push_stack(arg3,sp) in
                                    let sp = push_stack(arg2,sp) in
                                    let sp = push_stack(arg1,sp) in
                                    env[0] <- acc;
                                    let next_pc = long_val (get_field(acc,0)) in
                                    extra_args[0] <- extra_args[0] + 2;
                                    (next_pc,acc,sp))
          | 40 (* RETURN *) -> let n = code[pc_plus_1] in
                               let sp = sp - n in
                               if extra_args[0] > 0 then (
                                 extra_args[0] <- extra_args[0] - 1;
                                 env[0] <- acc;
                                 let next_pc = long_val (get_field(acc,0)) in
                                 (next_pc, acc, sp))
                               else (
                                 let (v,sp) = pop_stack(sp) in
                                 let next_pc = long_val v in (* long_val ?  next_sp-1 ? *)
                                 let (w,sp) = pop_stack(sp) in 
                                 env[0] <- w;
                                 let (u,sp) = pop_stack(sp) in 
                                 extra_args[0] <- long_val(u);
                                 (next_pc,acc,sp) 
                               )
          | 41 (* RESTART *) -> let v = env[0] in
                                let nbargs = size_val(v) - 2 in
                                let rec loop_push(sp,i) =
                                  if i >= nbargs then sp else 
                                  let sp = push_stack(get_field(v,i+2),sp) in
                                  loop_push(sp,i+1)
                                in
                                let sp = loop_push(sp,0) in
                                env[0] <- get_field(v,1);
                                extra_args[0] <- extra_args[0] + nbargs;
                                (pc_plus_1,acc,sp)
          | 42 (* GRAB *) -> on_argument (fun n -> 
                               let x = extra_args[0] in
                               if x >= n then ( extra_args[0] <- x - n; 
                                                (pc_plus_2,acc,sp) ) 
                               else ( 
                                 let next_acc = make_block(closure_tag,x + 3) in
                                 set_field(next_acc, 0, val_long (pc_plus_2 - 3));
                                 set_field(next_acc, 1, env[0]);
                                 let rec w(i,sp) =
                                   if i > x then sp else
                                   let (v,sp) = pop_stack(sp) in
                                   set_field(next_acc,i+2,v);
                                   w(i+1,sp) 
                                 in
                                 let sp = w(0,sp) in
                                 let (v,sp) = pop_stack(sp) in
                                 let next_pc = long_val v in 
                                 let (w,sp) = pop_stack(sp) in
                                 env[0] <- w;
                                 let (u,sp) = pop_stack(sp) in
                                 extra_args[0] <- long_val(u);
                                 (next_pc,next_acc,sp)))
          | 43 (* CLOSURE *) -> on_arguments2 (fun (n,ofs) ->
                                  let sp = if n > 0 then push_stack(acc,sp) else sp in
                                  let next_acc = make_closure(pc_plus_2+ofs,n+1) in
                                  let rec fill(i,sp) =
                                    if i >= n then sp else
                                    (let (v,sp) = pop_stack(sp) in
                                     set_field(next_acc,i,v); fill(i+1,sp))
                                  in 
                                  let sp = fill(1,sp) in
                                  (pc_plus_3, next_acc, sp))

          | 44 (* CLOSUREREC *) -> on_arguments3 (fun (f,v,o) ->
                                       let sp = if v > 0 then push_stack(acc,sp) else sp in
                                       let closure_size = (2 * f) - 1 + v in  
                                       let next_acc = make_block(closure_tag,closure_size) in
                                       set_field(next_acc, 0, val_long (pc_plus_3+o));
                                       let rec w0(i,sp) =
                                         if i >= v then sp else
                                         let (x,sp) = pop_stack(sp) in
                                         (set_field(next_acc, i + 2 * f - 1, x); w0(i+1,sp))
                                       in 
                                       let sp = w0(0,sp) in
                                       let rec w1(i) =
                                         if i >= f then () else
                                         (set_field(next_acc,2*i-1,make_header(infix_tag,2*i));
                                          set_field(next_acc,2*i,val_long(pc+2+code[pc_plus_3 + i]));
                                          w1(i+1))
                                       in 
                                       w1(1);
                                       let sp = push_stack(next_acc,sp) in
                                       let rec w3(i,sp) =
                                         if i >= f then sp else
                                         let sp = push_stack(val_ptr (ptr_val next_acc + (2 * i)),sp) in
                                         w3(i+1,sp)
                                       in
                                       let sp = w3(1,sp) in
                                       (pc_plus_3+f, next_acc, sp))

          | 45 (* OFFSETCLOSUREM2 *) -> offsetclosure_n(pc_plus_1, -2)
          | 46 (* OFFSETCLOSURE0 *) -> offsetclosure_n(pc_plus_1, 0)
          | 47 (* OFFSETCLOSURE2 *) -> offsetclosure_n(pc_plus_1, 2)
          | 48 (* OFFSETCLOSURE *) -> on_argument (fun n -> offsetclosure_n(pc_plus_2, n))

          | 49 (* PUSHOFFSETCLOSUREM2 *) -> pushoffsetclosure_n (pc_plus_1, -2)
          | 50 (* PUSHOFFSETCLOSURE0 *) -> pushoffsetclosure_n (pc_plus_1, 0)
          | 51 (* PUSHOFFSETCLOSURE2 *) -> pushoffsetclosure_n (pc_plus_1, 2)
          | 52 (* PUSHOFFSETCLOSURE *) -> on_argument (fun n -> pushoffsetclosure_n (pc_plus_2, n))
          | 53 (* GETGLOBAL *) -> on_argument (fun n ->
                                      let v = global_get n in 
                                      (pc_plus_2, v, sp))
          | 54 (* PUSHGETGLOBAL *) ->
                                      on_argument (fun n ->
                                        push_stack_implace(sp,acc);
                                        let v = global_get n in
                                        (pc_plus_2,v , sp_plus_1))
          | 55 (* GETGLOBALFIELD *) -> on_arguments2 (fun (n,p) ->
                                         let v = get_field(global_get n,p) in
                                         (pc_plus_2+1,v,sp))
          | 56 (* PUSHGETGLOBALFIELD *) -> on_arguments2 (fun (n,p) ->
                                            let sp = push_stack(acc,sp) in
                                            let v = get_field(global_get n,p) in
                                            (pc_plus_2+1,v,sp_plus_1))
          | 57 (* SETGLOBAL *) -> on_argument (fun n ->
                                    global_set(n,acc);
                                    (pc_plus_2,val_unit,sp))
          | 58 (* ATOM0 *) -> (pc_plus_1, make_block(0,0), sp)
          | 59 (* ATOM *) -> on_argument (fun tag -> 
                                let a = make_block(tag,0) in
                                (pc_plus_2, a,sp))
          | 60 (* PUSHATOM0 *) -> push_stack_implace(sp,acc);
                                  (pc_plus_1, make_block(0,0), sp_plus_1)
          | 61 (* PUSHATOM *) -> on_argument (fun tag -> 
                                    let sp = push_stack(acc,sp) in
                                    let a = make_block(tag,0) in
                                    (pc_plus_2,a,sp_plus_1))
          | 62 (* MAKEBLOCK *) -> let sz = code[pc_plus_1] in
                                  let tag = code[pc_plus_2] in 
                                  let blk = make_block(tag,sz) in
                                  set_field(blk,0,acc);
                                  let rec fill(i,sp) =
                                     if i >= sz then sp else
                                     let (v,sp) = pop_stack(sp) in
                                     (set_field(blk,i,v); fill(i+1,sp))
                                  in 
                                  let sp = fill(1,sp) in
                                  (pc_plus_3,blk,sp)
          | 63 (* MAKEBLOCK1 *) -> on_argument (fun tag ->
                                     let blk = make_block(tag,1) in
                                     set_field(blk,0,acc);
                                     (pc_plus_2,blk,sp))
          | 64 (* MAKEBLOCK2 *) -> on_argument (fun tag ->
                                     let blk = make_block(tag,2) in
                                     set_field(blk,0,acc);
                                     let (v,sp) = pop_stack(sp) in
                                     set_field(blk,1,v);
                                     (pc_plus_2,blk,sp))
          | 65 (* MAKEBLOCK3 *) -> let tag = code[pc_plus_1] in 
                                   let blk = make_block(tag,3) in
                                   set_field(blk,0,acc);
                                   set_field(blk,1,pop_stack_implace(sp_minus_1));
                                   let sp_minus_2 = sp_minus_2 in
                                   set_field(blk,2,pop_stack_implace(sp_minus_2));
                                   (pc_plus_2,blk,sp_minus_2)
         
         (* 66 MAKEFLOATBLOCK *)

          | 67 (* GETFIELD0 *) -> get_field_n(pc_plus_1, 0)
          | 68 (* GETFIELD1 *) -> get_field_n(pc_plus_1, 1)
          | 69 (* GETFIELD2 *) -> get_field_n(pc_plus_1, 2)
          | 70 (* GETFIELD3 *) -> get_field_n(pc_plus_1, 3)
          | 71 (* GETFIELD *) -> on_argument (fun n -> get_field_n (pc_plus_2, n))

          (* 72 GETFLOATFIELD *)

          | 73 (* SETFIELD0 *) -> set_field_n(pc_plus_1, 0)
          | 74 (* SETFIELD1 *) -> set_field_n(pc_plus_1, 1)
          | 75 (* SETFIELD2 *) -> set_field_n(pc_plus_1, 2)
          | 76 (* SETFIELD3 *) -> set_field_n(pc_plus_1, 3)
          | 77 (* SETFIELD *) -> on_argument (fun n -> set_field_n(pc_plus_2, n))

          (* 78 SETFLOATFIELD *)


          | 79 (* VECTLENGTH *) -> let nex_acc = val_long (size_val acc) in
                                  (pc_plus_1,nex_acc,sp)
          | 80 (* GETVECTITEM *) -> let (n,sp) = pop_stack(sp) in
                                    let v = get_field(acc,long_val(n)) in
                                     print_int v;
                                    (pc_plus_1, v, sp)
          | 81 (* SETVECTITEM *) -> let (n,sp) = pop_stack(sp) in
                                    let (v,sp) = pop_stack(sp) in
                                    set_field(acc,long_val(n),v);
                                    let next_acc = val_unit in
                                    (pc_plus_1,next_acc,sp)

            (* 82 GETSTRINGCHAR *)
            (* 83 SETSTRINGCHAR *)

          | 84 (* BRANCH *) -> (pc_plus_1+code[pc_plus_1], acc, sp)
          | 85 (* BRANCHIF *) -> let next_pc = if (is_ptr(acc) == 0)
                                                 && (long_val(acc) <> 0) 
                                                then pc_plus_1+code[pc_plus_1]
                                                else pc_plus_2
                                 in (next_pc, acc, sp)
          | 86 (* BRANCHIFNOT *) -> let next_pc = if (is_ptr(acc) == 0) 
                                                 && (long_val(acc) == 0) 
                                                then pc_plus_1 + code[pc_plus_1]
                                                else pc_plus_2
                                    in (next_pc, acc, sp)
          | 87 (* SWITCH *) -> on_argument (fun n ->
                                 let ofs = 
                                   if (is_int acc) == 1
                                   then long_val acc
                                   else let idx = tag_val acc in (n land 65535) + idx (* 65535 := 0xFFFF *)
                                 in 
                                 (pc_plus_2 + code[pc_plus_2+ofs], acc, sp))
          | 88 (* BOOLNOT *) -> (pc_plus_1,val_long (bnot (long_val (acc))), sp)
          | 89 (* PUSHTRAP *) -> on_argument (fun ofs -> 
                                   let sp = push_stack(val_long(extra_args[0]),sp) in
                                   let sp = push_stack(env[0],sp) in
                                   let sp = push_stack(trap_sp[0],sp) in
                                   let sp = push_stack(val_long(pc_plus_1+ofs),sp) in
                                   trap_sp[0] <- sp;
                                   (pc_plus_2, acc, sp))
          | 90 (* POPTRAP *) ->
              let sp = sp - 1 in
              let (next_trap_sp,sp) = pop_stack(sp) in
              trap_sp[0] <- next_trap_sp;
              let sp = sp - 2 in
              (pc_plus_1,acc,sp)
          | 91 (* RAISE *) -> if trap_sp[0] = (-1)   (* i.e., uncaught_exception *)
                              then (143,acc,sp) else 
                              let sp = trap_sp[0] in
                              let (next_pc,sp) = pop_stack(sp) in
                              let (next_trap_sp,sp) = pop_stack(sp) in
                              let (next_env,sp) = pop_stack(sp) in
                              let (next_extra_args,sp) = pop_stack(sp) in
                              trap_sp[0] <- next_trap_sp;
                              env[0] <- next_env;
                              extra_args[0] <- long_val(next_extra_args);   (* todo, check long_val everywhere *)
                              (long_val(next_pc),acc,sp)
          (* 92 CHECK-SIGNALS *)
          | 93 (* C-CALL1 *) -> on_argument (fun p -> 
                                  let sp = push_stack(env[0],sp) in
                                  let acc = call (p,acc) in
                                  let (v,sp) = pop_stack(sp) in
                                  env[0] <- v;
                                  (pc_plus_2,acc,sp))
          (* 94 C-CALL2 : unsupported *) 
          (* 95 C-CALL3 : unsupported *) 
          (* 96 C-CALL4 : unsupported *) 
          (* 97 C-CALL5 : unsupported *) 
          (* 98 C-CALLN : unsupported *) 
          | 99  (* CONST0 *) -> const_n(pc_plus_1, 0)
          | 100 (* CONST1 *) -> const_n(pc_plus_1, 1)
          | 101 (* CONST2 *) -> const_n(pc_plus_1, 2)
          | 102 (* CONST3 *) -> const_n(pc_plus_1, 3)
          | 103 (* CONSTINT *) -> on_argument (fun n -> const_n(pc_plus_2, n))
          | 104 (* PUSHCONST0 *) -> pushconst_n(pc_plus_1, 0)
          | 105 (* PUSHCONST1 *) -> pushconst_n(pc_plus_1, 1)
          | 106 (* PUSHCONST2 *) -> pushconst_n(pc_plus_1, 2)
          | 107 (* PUSHCONST3 *) -> pushconst_n(pc_plus_1, 3)
          | 108 (* PUSHCONSTINT *) -> on_argument (fun n -> pushconst_n(pc_plus_2, n))
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
          | 127 (* OFFSETINT *) -> let f(ofs) = val_long(addint(long_val acc, ofs)) in
                                   (pc_plus_2, on_argument f, sp) 
          | 128 (* OFFSETREF *) -> on_argument (fun ofs -> 
                                     let f0 = get_field(acc,0) in
                                     set_field(acc,0,val_long((long_val f0) + ofs));
                                     (pc_plus_2,val_unit,sp))
          | 129 (* ISINT *) -> (pc_plus_1, val_long (is_int(acc)), sp)
          (* 130 GETMETHOD *)
          | 131 (* BEQ *) -> compbranch((==))
          | 132 (* BNEQ *) -> compbranch((<>))
          | 133 (* BLTINT *) -> compbranch((<))
          | 134 (* BLEINT *) -> compbranch((<=))
          | 135 (* BGTINT *) -> compbranch((>))
          | 136 (* BGEINT *) -> compbranch((>=))
          | 137 (* ULTINT *) -> apply_acc_pop(ultint)
          | 138 (* UGEINT *) -> apply_acc_pop(ugeint)
          | 139 (* BULTINT *) -> branch_when_apply_acc_code(ultint)
          | 140 (* BUGEINT *) -> branch_when_apply_acc_code(ugeint)
          (* 141 GETPUBMET *)
          (* 142 GETDYNMET *)
          | 143 (* STOP *) -> print_string "STOP : "; (pc (* code.length*), acc, sp)
          | _ -> print_string "unknown opcode : ";
                 print_int (code[pc]);
                 print_newline ();
                 exit()
          end)
  in loop (0,0,stack_start) ;;

let main () = 
  init_data () ;
  load_code () ; 
  let pc = interp () in
  pc == 0 ;;
