type x = string

type tvar = V of string | T of ty
and ty = TInt of ty | TBool | TUnit
       | TTuple of ty list
       | TVar of tvar ref
       | TString of ty
       | TStatic of {elem:ty ; size: ty}
       | TSize of int

let new_tvar = let c = ref 0 in fun () -> incr c; TVar (ref (V ("'a"^string_of_int !c)))


type c = Unit
       | Int of {value:int;tsize:ty}
       | Bool of bool
       | Enum of x
       | String of string (* non synthesizable *)

type op = If (* i.e., a multiplexer *)
        | Add | Sub | Mult
        | Lt | Gt | Le | Ge | Eq | Neq
        | Div | Mod
        | And | Or | Not
        | GetTuple of
            (* (pos,arity,ty) *)
            (* ty is the type of the value from which a field is extracted  *)
             int * int * ty
        | To_string
        | TyConstr of ty
        | String_length of ty
        | Compute_address

type global = Static_array of c * int

type a = A_letIn of x * a * a
  | A_tuple of a list
  | A_const of c
  | A_var of x
  | A_call of op * a
  | A_string_get of x * x
  | A_buffer_get of x * a
  | A_buffer_length of x * ty (* [ty] is the size of the resulting integer *)

type write = Delayed | Immediate

type s = S_return of a
  | S_continue of x * a * int option (* None = continue sans changer d'instance (i.e. appel récursif terminal) *)
  | S_if of a * s * s option
  | S_case of a * (c * s) list
  | S_set of write * x * a
  | S_buffer_set of write * ty * x * x * x
  | S_seq of s * s
  | S_letIn of x * a * s
  | S_fsm of id * x * (x * s) list * s (* result * transition * start instruction *)
                * bool (* <- restart *)
  | S_print of a (* non synthesizable *)

and id = string
  (* // ... *)
  (* case *)

let set_ ?(write=Delayed) x a = S_set(write,x,a)
let seq_ s s' = S_seq(s,s')


module Debug = struct
  open Format

  let rec pp_ty fmt = function
  | TInt tz -> fprintf fmt "int<%a>" pp_ty tz
  | TBool -> fprintf fmt "bool"
  | TUnit ->  fprintf fmt "unit"
  | TTuple ts ->
      fprintf fmt "@[<v>(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt " * ") pp_ty fmt ts;
      fprintf fmt ")@]"
  | TVar{contents=V n} -> fprintf fmt "'a%s" n
  | TVar{contents=T t} -> pp_ty fmt t
  | TString ty -> fprintf fmt "string<%a>" pp_ty ty
  | TSize n -> fprintf fmt "size<%d>" n
  | TStatic {elem ; size} -> fprintf fmt "%a buffer<%a>" pp_ty elem pp_ty size

  let pp_c fmt = function
    | Unit -> fprintf fmt "()"
    | Int{value=n;tsize=t} -> fprintf fmt "%d'%ab" n pp_ty t
    | Bool b -> fprintf fmt "\"%d\"" (if b then 1 else 0)
    | Enum x -> fprintf fmt "%s" x
    | String s -> fprintf fmt "\"%s\"" s

  let pp_op fmt = function
  | If -> fprintf fmt "mixc_if"
  | Add -> fprintf fmt "mixc_add"
  | Sub -> fprintf fmt "mixc_sub"
  | Mult -> fprintf fmt "mixc_mult"
  | Eq -> fprintf fmt "mixc_eq"
  | Lt -> fprintf fmt "mixc_lt"
  | Le -> fprintf fmt "mixc_le"
  | Gt -> fprintf fmt "mixc_gt"
  | Ge -> fprintf fmt "mixc_ge"
  | And -> fprintf fmt "mixc_and"
  | Or -> fprintf fmt "mixc_or"
  | Not -> fprintf fmt "mixc_not"
  | Neq -> fprintf fmt "mixc_neq"
  | Div -> fprintf fmt "mixc_div"
  | Mod -> fprintf fmt "mixc_mod"
  | GetTuple (i,_,_) -> fprintf fmt "mixc_get_%d" i
  | To_string -> fprintf fmt "mixc_simul_to_string"
  | TyConstr _ -> fprintf fmt "mixc_id"
  | String_length ty -> fprintf fmt "mixc_string_length"
  | Compute_address -> fprintf fmt "mixc_compute_address"

  let rec pp_a fmt = function
  | A_const c -> pp_c fmt c
  | A_var x -> fprintf fmt "%s" x
  | A_call(op,a) ->
     fprintf fmt "@[%a(%a)@]" pp_op op pp_a a
  | A_letIn(x,a1,a2) ->
     fprintf fmt "@[<v>let %s = %a in@,%a@]" x pp_a a1 pp_a a2
  | A_tuple aas ->
      fprintf fmt "@[<v>(";
      pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") pp_a fmt aas;
      fprintf fmt ")@]"
  | A_string_get(sx,ix) -> fprintf fmt "%s[%s]" sx ix
  | A_buffer_get(x,idx) -> fprintf fmt "%s[%a]" x pp_a idx
  | A_buffer_length(x,_) -> fprintf fmt "%s.length" x


  let rec pp_s fmt = function
  | S_return a ->
      fprintf fmt "return %a" pp_a a
  | S_continue(f,a,id) ->
      let pp_id fmt = function
      | None -> fprintf fmt "none"
      | Some n -> fprintf fmt "%d" n in
    fprintf fmt "continue %s[%a](%a)" f pp_id id pp_a a
  | S_if(a,s,so) ->
      fprintf fmt "@[<v 2>if %a(0) = '1' then@,%a@]@," pp_a a pp_s s;
      Option.iter (fun s' ->
        fprintf fmt "@[<v 2>else@,%a@]@,end if;" pp_s s') so
  | S_case(a,hs) ->
      fprintf fmt "@[<v>case %a is@," pp_a a;
      List.iter (fun (c,s) -> fprintf fmt "@[<v 2>when %a =>@,%a@]@," pp_c c pp_s s) hs;
      fprintf fmt "@]end case;";
  | S_set(w,x,a) ->
      let op = match w with Delayed -> "<=" | Immediate -> ":=" in
      fprintf fmt "@[<v>%s %s %a;@]" x op pp_a a
  | S_buffer_set(w,ty_,x,idx,y) ->
      let op = match w with Delayed -> "<=" | Immediate -> ":=" in
      fprintf fmt "@[<v>%s(%s) %s %s;@]" x idx op y
  | S_seq(s1,s2) ->
      fprintf fmt "@[<v>%a@,%a@]" pp_s s1 pp_s s2
  | S_letIn(x,a,s) ->
      fprintf fmt "@[<v>let %s = %a in@,%a@]" x pp_a a pp_s s
  | S_fsm(_,result,ts,s,b) ->
      fprintf fmt "@[<v>((%s) -> %a)%s@]" result pp_fsm (ts,s) (if b then "[restart]" else "")
  | S_print(a) ->
      fprintf fmt "mixc_print(%a)" pp_a a

  and pp_fsm fmt (ts,s) =
    let pp_t fmt (x,s) = fprintf fmt "%s = %a@]@," x pp_s s in
    fprintf fmt "@[<v>let rec ";
    pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,and ") pp_t fmt ts;
    fprintf fmt "@,@]in %a" pp_s s

end
