(* THIS FILE HAS BEEN GENERATED *)

let init_data () = 
let x1 = 10000 in
(* ADD GLOBAL 12 *)
ram[global_start + 12] <- val_long(0);
  ()
 ;;

let call (n,arg) =
  match n with
  | 0 -> caml_print_int(arg)
  | _ -> print_string "unknown primitive"; print_int n; exit () end ;;


let static code = 0^43 ;;

let load_code () = 
  code[0] <- 84;
  code[1] <- 16;
  code[2] <- 41;
  code[3] <- 42;
  code[4] <- 3;
  code[5] <- 53;
  code[6] <- 12;
  code[7] <- 14;
  code[8] <- 14;
  code[9] <- 14;
  code[10] <- 14;
  code[11] <- 110;
  code[12] <- 110;
  code[13] <- 110;
  code[14] <- 110;
  code[15] <- 40;
  code[16] <- 4;
  code[17] <- 103;
  code[18] <- 1000;
  code[19] <- 57;
  code[20] <- 12;
  code[21] <- 43;
  code[22] <- 0;
  code[23] <- -20;
  code[24] <- 9;
  code[25] <- 31;
  code[26] <- 12;
  code[27] <- 103;
  code[28] <- 300;
  code[29] <- 108;
  code[30] <- 100;
  code[31] <- 108;
  code[32] <- 200;
  code[33] <- 108;
  code[34] <- 42;
  code[35] <- 17;
  code[36] <- 32;
  code[37] <- 4;
  code[38] <- 93;
  code[39] <- 0;
  code[40] <- 19;
  code[41] <- 1;
  code[42] <- 143;
  ()
 ;;

