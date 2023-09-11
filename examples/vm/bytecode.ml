(* THIS FILE HAS BEEN GENERATED *)

let init_data () = 
let x1 = 10000 in
  ()
 ;;

let call (n,arg) =
  match n with
  | 0 -> caml_print_int(arg)
  | _ -> print_string "unknown primitive"; val_unit
  end ;;


let static code = (0:long)^5 ;;

let load_code () = 
  code[0] <- 103;
  code[1] <- 42;
  code[2] <- 93;
  code[3] <- 0;
  code[4] <- 143;
  ()
 ;;

