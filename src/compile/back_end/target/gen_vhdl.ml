open Fsm_syntax
open Format

let size_ty t = 
  Fsm_typing.size_ty t

(** [size_const c] returns the number of bits of constant [c] *)
let size_const c =
  match c with
  | Unit | Bool _ ->
      1
  | Int {value=_;tsize} -> 
      size_ty tsize
  | Enum _ -> 
      assert false (* cannot infer enum size *)
  | String s -> String.length s * 8

(* [reserved x] returns [true] iff [x] is a VHDL keyword 
   or a reserved identifier (e.g., reset) *)
let reserved : string -> bool = 
  let tbl = Hashtbl.create 20 in
  let () = 
    List.iter (fun x -> Hashtbl.add tbl x ()) @@
      [ "_"; "reset"; "others"; "run" ; "t_state"; "value"; "clk"; "loop"] 
      (* todo: complete with other VHDL keywords *)
  in
  (fun x -> Hashtbl.mem tbl x)

(** [norm_ident x] convert [x] to a valid VHDL identifier *)
let norm_ident x =
  let is_azAZ_19 c =
    (c >= '0' && c <= '9') ||
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') || c = '_' 
  in
  let exception E in
  try
    if reserved x then raise E;
    String.iter (fun c -> if not (is_azAZ_19 c) then raise E) x;
    x
  with E -> "\\" ^ x ^"\\" (* extended VHDL identifier: anything between backslash *)

(** code generator for identifiers *)
let pp_ident fmt (x:x) : unit =
    fprintf fmt "%s" (norm_ident x)


(** code generator for constants *)
let pp_c fmt c =
  match c with
  | Unit -> fprintf fmt "\"1\""
  | Int {value=n;tsize} ->
      let v = Printf.sprintf "%x" n in
      let l_pad = size_ty tsize - String.length v * 4 in
      if l_pad < 0 then
      begin
        assert false (** should not happen ! *)
      end;
      fprintf fmt "\"%s\" & X\"%s\"" (String.make l_pad '0') v
  | Bool b -> fprintf fmt "\"%d\"" (if b then 1 else 0)
  | Enum x -> pp_ident fmt x
  | String s -> fprintf fmt "of_string(\"%s\")" s

(** code generator for operator *)
let pp_op fmt = function
| If -> fprintf fmt "mixc_if"
| Add -> fprintf fmt "mixc_add"
| Sub -> fprintf fmt "mixc_sub"
| Mult -> fprintf fmt "mixc_mult"
| Eq -> fprintf fmt "mixc_eq"
| Neq -> fprintf fmt "mixc_neq"
| Lt -> fprintf fmt "mixc_lt"
| Le -> fprintf fmt "mixc_le"
| Gt -> fprintf fmt "mixc_gt"
| Ge -> fprintf fmt "mixc_ge"
| And -> fprintf fmt "mixc_and"
| Or -> fprintf fmt "mixc_or"
| Not -> fprintf fmt "mixc_not"
| Div -> fprintf fmt "mixc_div"
| Mod -> fprintf fmt "mixc_mod"
| TyConstr _ -> fprintf fmt "mixc_id"
| To_string -> fprintf fmt "mixc_to_string"
| GetTuple (i,_,_) -> assert false (* special case, defined below (see tuple_access) *)
| String_length _ -> assert false (* deal with in pp_call*)
| Compute_address -> assert false (* deal with in pp_call*)


(** code generator for tuples deconstruction *)
let rec pp_tuple_access fmt (i:int) ty (a:a) : unit =

  let rec tuple_access i ty_a a =
      (* compute bounds of the value to be accessed at index [i_to_find]
         among projections of types ts *)
    let slice_bounds i_to_find ts =
      let rec aux j acc = function
      | [] -> assert false
      | t::ts' -> if i_to_find = j then acc,t,ts' else aux (j+1) (t::acc) ts' in
      aux 0 [] ts
      in
      let open Fsm_typing in
      match a,canon ty_a with
      | A_tuple aas,TTuple ts ->
          `Atom(List.nth aas i)
      | A_call(GetTuple(j,_,ty2),a2),TTuple ts ->
          let ts_before,t,_ = slice_bounds i ts in
          (match tuple_access j ty2 a2 with
          | `Slice(a,i1,i2) ->
                let z = size_ty (TTuple ts_before) in
                let tz = size_ty t in
                `Slice(a,z+i1,z+i1+tz-1)
          | `Atom _ as a -> a)
      | _,TTuple ts ->
            let ts_before,t,ts_after = slice_bounds i ts in
            let z' = size_ty (TTuple ts_after) in
            let tot = size_ty (TTuple ts) in
            let j = tot-z'-size_ty t in
            let k = tot - z'-1 in
            `Slice(a,j,k)
      | _ -> assert false
  in

  match tuple_access i ty a with
  | `Slice(a,j,k) ->
      let pp_slice fmt (j,k) =
         fprintf fmt "%d to %d" j k
      in
      fprintf fmt "%a(%a)" pp_a a pp_slice (j,k)
  | `Atom(a) -> pp_a fmt a


(** code generator for call of operator *)
and pp_call fmt op a =
  match op with
  | GetTuple(i,_,ty) -> pp_tuple_access fmt i ty a
  | String_length ty -> pp_c fmt (Int {value=(size_ty ty (* / 8*));tsize=TSize(32)})
  | Compute_address -> fprintf fmt "mixc_compute_address(caml_heap_base,%a)" pp_a a
  | _ -> fprintf fmt "@[%a(%a)@]" pp_op op pp_a a


(** code generator for atoms (i.e. combinatorial expression) *)
(* assumes that the let-bindings of atoms are not nested *)
and pp_a fmt = function
| A_const c -> pp_c fmt c
| A_var x -> fprintf fmt "%a" pp_ident x
| A_call(op,a) ->
   pp_call fmt op a
| A_letIn(x,a1,a2) ->
   fprintf fmt "@[%a := %a;@,%a@]" pp_ident x pp_a a1 pp_a a2
| A_tuple aas -> pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " & ") pp_a fmt aas
| A_string_get(s,i) -> fprintf fmt
    "@[%a(to_integer(unsigned(%s&\"000\")) to to_integer(unsigned(%s&\"000\"))+7)@]" pp_ident s i i
| A_buffer_get(xb,idx) ->
    fprintf fmt  "%a(to_integer(unsigned(%a)))" pp_ident xb pp_a idx
| A_buffer_length(x,tz) ->
    fprintf fmt  "std_logic_vector(to_unsigned(%a'length,%d))" pp_ident x (size_ty tz)

(** code generator for statements *)
let rec pp_s fmt = function
| (S_return _ | S_continue _) -> assert false (* already expanded *)
| S_if(a,s1,so) ->
    fprintf fmt "@[<v 2>if %a(0) = '1' then@,%a@]" pp_a a pp_s s1;
    Option.iter (fun s2 -> fprintf fmt "@,@[<v 2>else@,%a@]" pp_s s2) so;
     fprintf fmt "@,end if;"
| S_case(a,hs) ->
    fprintf fmt "@[<v>case %a is@," pp_a a;
    List.iter (fun (c,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_c c pp_s s) hs;
    fprintf fmt "@]end case;";
| S_set(Delayed,x,a) -> fprintf fmt "@[<v>%a <= %a;@]" pp_ident x pp_a a
| S_set(Immediate,x,a) -> fprintf fmt "@[<v>%a := %a;@]" pp_ident x pp_a a

| S_buffer_set(_,_,x1,x2,x3) ->
    fprintf fmt
    "@[%a(to_integer(unsigned(%a))) := %a;@]" pp_ident x1 pp_ident x2 pp_ident x3

| S_seq(s1,s2) -> fprintf fmt "@[<v>%a@,%a@]" pp_s s1 pp_s s2
| S_letIn(x,a,s) -> fprintf fmt "@[<v>%a := %a;@,%a@]" pp_ident x pp_a a pp_s s
| S_fsm(id,x,ts,s,b) ->
     let (st,cp,_) = List.assoc id !Encode.extra_machines in
     pp_fsm fmt ~restart:b ~state_var:st ~compute:cp (id,ts,s)
| S_print(a) ->
     fprintf fmt "report to_string(%a);@," pp_a a

(** code generator for FSMs *)
and pp_fsm fmt ~restart ~state_var ~compute (id,ts,s) =
    if restart then (
      fprintf fmt "@[<v>case %a is@," pp_ident state_var;
      List.iter (fun (x,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_ident x pp_s s) ts;
      fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_ident compute pp_s s;
      fprintf fmt "@]end case;")
    else (
    fprintf fmt "@[<v>case %a is@," pp_ident state_var;
    List.iter (fun (x,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_ident x pp_s s) ts;
    fprintf fmt "@[<v 2>when %a =>@,%s \"0\";@,%a@]@," pp_ident compute (if id="main" then "rdy <=" else id^"_rdy :=") pp_s s;
    fprintf fmt "@]end case;"
   )


(* in fact, it is not the let-bindings that we are looking for,
but the VHDL variables in the code to generate, in order to infer the declarations *)
let rec list_let_bindings (ts,s) =
  (* assume : no let-bindings in atoms (eliminated before) *)
  let rec accum = function
  | S_continue _ -> []
  | S_set(Immediate,x,_) -> [x]
  | S_set(Delayed,_,_) -> []
  | S_buffer_set(Immediate,_,x,_,_) -> [x]
  | S_buffer_set(Delayed,_,_,_,_) -> []
  | S_if(_,s1,so) -> accum s1 @ (match so with None -> [] | Some s2 -> accum s2)
  | S_seq(s1,s2) -> accum s1 @ accum s2
  | S_case(_,hs) -> List.map (fun (_,s) -> accum s) hs |> List.concat
  | S_letIn(x,_,s) -> x::accum s
  | S_return _ -> []
  | S_fsm(_,_,ts,s,b) ->
      list_let_bindings (ts,s)
  | S_print _ -> []
in List.concat (List.map (fun (_,s) -> accum s) ts) @ accum s

(* default value as bitvector where each bit is at '0' *)
let default_zero_value nbits =
  "(others => '0')"

(* default value according to the given type. *)
let default_zero t =
  match Fsm_typing.canon t with
  | TStatic _ -> "(others => (others => '0'))"
  | _ -> "(others => '0')"

  (* let bits_in_hexa = nbits / 4 in
  let bits_in_binary = nbits mod 4 in
  let make ?(hexa=false) n =
    "\""^(String.make n '0')^"\""
  in
  match bits_in_binary,bits_in_hexa  with
  | 0,n -> "X"^make n
  | n,0 -> make n
  | n,m -> make n^"& X"^make m *)

  (* else
    "X\""^(String.make (int_of_float (Float.ceil (float nbits /. 4.))) '0')^"\"" (* incorrect: pour 33 bits par exemple *)
*)

let qualify prefix y =
  prefix^"_"^y

let declare_state_var fmt state_var compute xs =
  let state_var_tname = Naming_convention.state_var_type state_var in
    fprintf fmt "type %a is (%a" pp_ident state_var_tname pp_ident compute;

    List.iter (fun x -> fprintf fmt ", %a" pp_ident x) xs;

    fprintf fmt ");@,signal %a: %a;@," pp_ident state_var pp_ident state_var_tname



let declare_machine fmt ~state_var ~compute ~infos (ts,s) =

  declare_state_var fmt state_var compute (List.map fst ts);

  List.iter (fun (_,(sv,cp,xs)) -> declare_state_var fmt sv cp xs) !Encode.extra_machines;

  Fsm_comp.SMap.iter (fun x w ->
    let inst_tname = Naming_convention.instances_type x in
    let sq = Fsm_comp.IMap.to_seq w in
    let l = (List.of_seq sq) in
    begin
      fprintf fmt "type %a is (" pp_ident inst_tname ;
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ")
        (fun fmt (n,_) -> fprintf fmt "%a" pp_ident (Naming_convention.instance_enum_const n)) fmt l;
      fprintf fmt ");@,";
      fprintf fmt "signal %a : %a;@," pp_ident (Naming_convention.instance_id_of_fun x) pp_ident inst_tname
    end
  ) infos
(* type array_value is array (0 to 20) of value(0 to 31); *)
let pp_ty fmt t =
  match Fsm_typing.canon t with
  | TStatic{elem;size} -> fprintf fmt "array_value_%d(0 to %d)" (size_ty elem) (size_ty size - 1);
  | _ ->
      fprintf fmt "value(0 to %d)" (size_ty t-1)




module ArrayType = Map.Make(struct
    type t = int let compare = Stdlib.compare
  end)


(* code generator for the whole design *)
let pp_component fmt ~name ~state_var ~argument ~result ~compute ~rdy ~statics typing_env infos (ts,s) =

  let arty = List.fold_left (fun arty (_,g) ->
      match g with
      | Static_array(c,_) -> ArrayType.add (size_const c) () arty) ArrayType.empty statics
  in

  Fsm_comp.SMap.iter (fun x _ -> Hashtbl.remove typing_env x;
                       Hashtbl.remove typing_env (Naming_convention.instance_id_of_fun x)) infos;


  let vars = list_let_bindings (ts,s) in

  fprintf fmt "@[<v>library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;

@[<v 2>entity %a is@," pp_ident name;

  let t_argument = Hashtbl.find_opt typing_env argument in
  let t_result = Hashtbl.find_opt typing_env result in
  if t_argument = None || t_result = None then
    (fprintf fmt "generic(@[<v>";
     if t_argument = None then fprintf fmt "argument_width : natural := 1";
     if t_argument = None && t_result = None then fprintf fmt ";@,";
     if t_result = None then fprintf fmt "  result_width : natural := 1@,";
     fprintf fmt ");@]");
  fprintf fmt "@,port(@[<v>signal clk    : in std_logic;@,";
  fprintf fmt "signal reset  : in std_logic;@,";
  fprintf fmt "signal run    : in std_logic;@,";
  fprintf fmt "signal %s    : out value(0 to 0);@," rdy;
  let st_argument = match t_argument with None -> "argument_width - 1" | Some t -> string_of_int (size_ty t - 1) in
  let st_result = match t_result with None -> "result_width - 1" | Some t -> string_of_int (size_ty t - 1) in
  fprintf fmt "signal %s : in value(0 to %s);@," argument st_argument;
  fprintf fmt "signal %s : out value(0 to %s)" result st_result;

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt ";@,signal caml_heap_base : in value(0 to 31)";
  end;
  fprintf fmt ");@,@]@]@,end entity;
architecture rtl of %a is@,@[<v 2>@," pp_ident name;


  declare_machine fmt ~state_var ~compute ~infos (ts,s);

  ArrayType.iter (fun n _ ->
      fprintf fmt "type array_value_%d is array (natural range <>) of value(0 to %d);@," n (n-1)) arty;

  Hashtbl.iter (fun x t ->
      if x <> result && x <> argument then
        if not (List.mem x vars) then
          fprintf fmt "signal %a : %a;@," pp_ident x pp_ty t
    ) typing_env;

  fprintf fmt "@,@[<v 2>begin@,@[<v 2>process(reset, clk)@,";

  begin
    let var_decls = Hashtbl.create 10 in
    let add_var x n =
      match Hashtbl.find_opt var_decls n with
      | None -> Hashtbl.add var_decls n [x]
      | Some s -> Hashtbl.replace var_decls n (x::s)
    in
    Hashtbl.iter (fun x t ->
        if x <> result && x <> argument then
          if (List.mem x vars) then
            add_var x (size_ty t)
      ) typing_env;

     Hashtbl.iter (fun n xs ->
        fprintf fmt "variable @[<v>@[<hov>%a@]@,: value(0 to %d);@]@," 
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ @,") pp_ident) xs (n-1)
      ) var_decls;
  end;

  fprintf fmt "@]@,@[<v 2>begin@,@[<v 2>if (reset = '1') then@,";


  fprintf fmt "@[<hov>";
   Hashtbl.iter (fun x t ->
      match List.assoc_opt x statics with
      | Some (Static_array(c,n)) ->
          fprintf fmt "@]@,%a <= (others => %a);@,@[<hov>" pp_ident x pp_c c
      | None ->
          if x <> argument then
            let d = default_zero t in
            if not (List.mem x vars) then
              fprintf fmt "@]@,%a <= %s;@,@[<hov>" pp_ident x d
            else fprintf fmt "default_zero(%a);@ @," pp_ident x
    ) typing_env;
  fprintf fmt "@]";

  fprintf fmt "@,rdy <= \"1\";@,";
  fprintf fmt "state <= Compute;@,";

  List.iter (fun (_,(sv,cp,xs)) -> fprintf fmt "%a <= %a;@," pp_ident sv pp_ident cp) !Encode.extra_machines;

  fprintf fmt "@]@,@[<v 2>elsif rising_edge(clk) then@,";

  fprintf fmt "@[<v 2>if run = '1' then@,";

  pp_fsm ~restart:false fmt ~state_var ~compute ("main",ts,s);

  fprintf fmt "@]@,end if;
    end if;
  end process;
end architecture;@]\n";

  ( (argument,t_argument), (result,t_result) )

