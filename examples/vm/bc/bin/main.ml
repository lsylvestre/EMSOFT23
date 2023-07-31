open OByteLib


let gensym =
  let c = ref 0 in
  (fun () -> incr c; !c)
 
let rec add_val oc i v =
  (let open Value in
        match v with
        | Int n -> "val_long("^string_of_int n^")";
        | Block (tag,vs) ->
           let len = Array.length vs+1 in
           let x_num = gensym () in
           Printf.fprintf oc "(* ========= *)\n";
           Printf.fprintf oc "ram[x%d] <- make_header(%d,%d);\n" x_num tag (len-1);
           Printf.fprintf oc "let x%d = x%d+%d in\n" (x_num+1) x_num len;
           Array.iteri (fun j v ->
              let res = add_val oc i v in
              Printf.fprintf oc "ram[x%d+%d] <- %s;\n" x_num (j+1) res) vs;
         
           "val_ptr(x" ^ string_of_int(x_num) ^ "+1)"
        | String _ -> "val_long("^ string_of_int !i ^")" (* todo *) 
        | _ -> failwith "unsupported value type"
        );;

let write_data oc data =
  Printf.fprintf oc "let init_data () = \n";
  let i = ref 0 in
  Printf.fprintf oc "let x1 = 10000 in\n";
  Array.iter (fun v ->
      if !i >= 12 then
        (Printf.fprintf oc "(* ADD GLOBAL %d *)\n" !i;
        Printf.fprintf oc "ram[global_start + %d] <- %s;\n" !i  (add_val oc i v));
        incr i;
    ) data;
  Printf.fprintf oc "  ()\n ;;\n\n"

(* ********************************** *)

let write_prim oc prim =
  
  Printf.fprintf oc "let call (n,arg) =\n" ;
  Printf.fprintf oc "  match n with\n";
  Array.iteri (fun i s -> Printf.fprintf oc "  | %d -> %s(arg)\n" i s) prim;
  Printf.fprintf oc "  | _ -> print_string \"unknown primitive\"; print_int n; exit () end ;;\n\n\n"
(* ********************************** *)

let process ~src oc ?(pre=(fun _ _ -> ())) f ?(post=(fun _ -> ())) () =
  let ic = open_in src in
  let b = Bytes.create 4 in
  let i = ref 0 in
  pre oc (in_channel_length ic);
  try while true do
    really_input ic b 0 4;
    (* let n = Bytes.get_int16_le b 0 in*)
    (* let n = Int32.(to_int (if compare n zero >= 0 then n else neg n)) in*)
    f oc !i b;
    incr i
  done with _ -> ();
  post oc;
  close_in ic


let write_code ~version oc code =
  let tmp_code_path = "tmp/code.txt" in
  let oc_tmp = open_out tmp_code_path in
  Code.write version oc_tmp code;
  close_out oc_tmp;
  process ~src:tmp_code_path oc
    ~pre:(fun oc len -> Printf.fprintf oc "let static code = 0^%d ;;\n\nlet load_code () = \n" (len/4))
    (fun oc i b -> 
        let n = Bytes.get_int16_le b 0 in
        Printf.fprintf oc "  code[%d] <- %d;\n" i n)
    ~post:(fun oc -> Printf.fprintf oc "  ()\n ;;\n\n")
    ()



let main () =
  let inpath =
    match Sys.argv with
    | [| _; inpath |] -> inpath
    | _ -> failwith "unknown path"
  in
  
  let bytefile = Bytefile.read inpath in

  (* Bytefile.print stdout bytefile;*)

  let Bytefile.{version;code;data;prim;_} = bytefile in
  
  let oc = open_out "../bytecode.ml" in
  Printf.fprintf oc "(* THIS FILE HAS BEEN GENERATED *)\n\n";
  write_data oc data;
  write_prim oc prim;
  write_code ~version oc code;
  close_out oc
  


let () = main ()