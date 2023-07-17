
type ty =
  | T_const of tconst
  | T_var of tvar ref
  | T_tuple of ty list
  | T_fun of {arg:ty;dur:ty;ret:ty} (* function types annotated with response time *)
  | T_array of ty
  | T_string of ty (* string parameterized by their size-type *)
  | T_static of {elem : ty ; size : ty} (* buffer parameterized by their size-type *)

  (* sized types for check response time and bound of static datastructures *)
  | T_size of int      (* n *)
  | T_infinity         (* plus infinite *)
  | T_add of ty * ty   (* t + t'  *)
  | T_max of ty * ty   (* max(t,t') *)
  | T_le of ty * ty    (* t such that t <= t' *)

and tconst =
| TBool
| TInt of ty (* sized integer *)
| TUnit

and tvar =
  | Unknown of int
  | Ty of ty


type deco = Prelude.loc

type x = string         (** identifier [x] *)

type l = string         (** location [l] *)

type c =                (** constant [c] *)
  | Unit                (** unit value [()] *)
  | Bool of bool        (** boolean [true | false] *)
  | Int of int * ty     (** integer [n] of given size *)
  | String of string    (** string [s] *)
  | Op of op            (** primitive [op] *)
  | External of extern  (** asynchronous primitive *)
  | V_loc of l          (** pointer [l], only for interpretation *)

(* primitives *)

and op = Add | Sub | Mult | Div | Mod
       | Lt | Le | Gt | Ge | Eq | Neq
       | And | Or | Not | Abs
       | GetTuple of {pos : int; arity : int}
       | Wait of int
       | To_string
       | TyConstr of ty
       | String_length
         (* for simulation *)
       | Assert | Print | Random

(* exemples of primitives manipulating data structures in shared memoy *)
and extern =
  | Array_make
  | Array_set
  | Array_get
  | Array_length

type p = P_unit
       | P_var of x
       | P_tuple of p list

(*   | E_fun of p * e * bool     (** function [fun p -> e] *)
  | E_fnode of p * p * e * int(** node [node p => p with e] *) *)

type e =                      (** expression     [e]                     *)
    E_deco of e * deco        (** annot an expression with its location  in the source code *)
  | E_const of c              (** constant       [c]                     *)
  | E_var of x                (** variable       [x,y,f,g ...]           *)
  | E_app of e * e            (** application    [e1 e2]                 *)
  | E_tuple of e list         (** tuple          [e1, ... en]            *)
  | E_letIn of p * e * e      (** let-bindings   [let p = e1 in e2]      *)
  | E_if of e * e * e         (** conditional    [if e1 then e2 else e3] *)
  | E_fun of p * e            (** function       [fun p -> e]            *)
  | E_fix of x * (p * e)      (** recursive function [fix (fun p -> e)]  *)
  | E_reg of v * e            (** register       [reg f last e]          *)
  | E_exec of e * e * x       (** exec           [(exec e default e)^x]  *)
  | E_lastIn of x * e * e     (** local variable [last x = e in e]       *)
  | E_set of x * e            (** assignment     [x <- e]                *)
  | E_static_array_get of x * e     (** static array access     [x[e]]      *)
  | E_static_array_length of x      (** static array length access x.length *)
  | E_static_array_set of x * e * e (** static array assignment [x[e] <- e] *)
  | E_step of e * x           (** spawn          [(step e)^x]            *)
  | E_par of e * e            (** parallel       [e1 || e2]              *)

and v = V of e

type static =
| Static_array of c * int

type pi = {
  statics : (x * static) list ;
  ds : (x * e) list;
  main : e
}

(** [pat_mem x p] returns true iff [x] is bound in [p] *)
let pat_mem (x:x) (p:p) : bool =
  let exception Found in
  (** [aux p] raise [Found] when [x] is found in [p] *)
  let rec aux = function
    | P_unit -> ()
    | P_var(y) -> if x = y then raise Found
    | P_tuple(ps) -> List.iter aux ps
  in
  try aux p; false with Found -> true

(** [group_ps ps] builds a pattern from a list of patterns *)
let group_ps (ps:p list) : p =
  match ps with
  | [] -> P_unit
  | [p] -> p
  | ps -> P_tuple ps

(** [group_es es] builds an expression from a list of expressions *)
let group_es (es:e list) : e =
  match es with
  | [] -> E_const Unit
  | [e] -> e
  | es -> E_tuple es

(** [group_ts ts] builds a type from a list of types *)
let group_ts (ts:ty list) : ty =
  match ts with
  | [] -> T_const TUnit
  | [t] -> t
  | ts -> T_tuple ts

let gen_int : unit -> int =
  let c = ref 0 in
  fun () -> incr c; !c

(** symbol generator *)

let gensym : ?prefix:string -> unit -> x =
  let c = ref 0 in
  (fun ?(prefix="") () ->
    incr c; "v"^prefix^string_of_int !c)

module SMap = Map.Make(String)

type 'a env = 'a SMap.t

(* let (++) m1 m2 =
  SMap.union (fun _ _ -> assert false) m1 m2 *)
let (++) m1 m2 =
  SMap.union (fun _ _ v2 -> Some v2) m1 m2

let smap_of_list l =
  List.fold_right (fun (x,v) m -> SMap.add x v m) l SMap.empty

let rec vars_of_p = function
  | P_unit -> SMap.empty
  | P_var x -> SMap.singleton x ()
  | P_tuple ps ->
    List.fold_left (fun m p -> vars_of_p p ++ m) SMap.empty ps

let fun_ p e =
  E_fun(p,e)

let funfix_ (f:x) (p:p) (e:e) : e =
  E_fix(f,(p,e))


let rec un_deco (e:e) : e =
  match e with
  | E_deco(e,_) -> e
  | e -> e

(* [un_TyConstr e] removes both decorations
   and type constraints around expression [e] *)
let rec un_TyConstr (e:e) : e =
  match e with
  | E_deco(e,_) -> un_TyConstr e
  | E_app(E_const(Op(TyConstr _)),e) -> un_TyConstr e
  | e -> e

let rec un_annot (e:e) : e =
  match e with
  | E_deco(e,_) -> un_annot e
  | E_app(E_const(Op(TyConstr _)),e) -> un_annot e
  | e -> e


let is_constant (e:e) : bool =
  match un_TyConstr (un_deco e) with
  | E_const _ -> true
  | _ -> false

let is_variable (e:e) : bool =
  match un_TyConstr (un_deco e) with
  | E_var _ -> true
  | _ -> false

let as_variable (e:e) : x =
  match un_TyConstr e with
  | E_var x -> x
  | _ -> assert false


(* [evaluated e] returns [true] if [e] is a variable *)
let rec evaluated (e:e) : bool =
  match un_TyConstr (un_deco e) with
  | E_const _ | E_fun _ | E_fix _ -> true
  | E_tuple es -> List.for_all evaluated es
  | E_app(E_const(Op(TyConstr _)),e) -> evaluated e
  | _ -> false

(* [loc_of e] returns the location arround [e] if it exists, or a default location *)
let rec loc_of (e:e) : Prelude.loc =
  match e with
  | E_deco(_,loc) ->
      loc
  | e -> Prelude.dloc

(* [mk_loc loc e] plugs the expression [e] into a node indicating the location [loc] *)
let mk_loc (loc: Prelude.loc) (e:e) : e =
  E_deco(e,loc)

(* [ty_annot ~ty e] plugs the expression [e] under a type constraints such as [(e:ty)] *)
let ty_annot ~(ty:ty) (e:e) : e =
  E_app(E_const (Op (TyConstr ty)),e)

(* like [ty_annot] with an optional argument [~ty] *)
let ty_annot_opt ~(ty:ty option) (e:e) : e =
  match ty with
  | None -> e
  | Some ty ->ty_annot ~ty e

(* static expressions *)
let rec static (e:e) : bool =
  let static_aux e =
    match e with
    | E_app (E_const (Op (GetTuple _)),e) -> static e
    | _ -> true
  in
  is_variable e || evaluated e || static_aux e

exception CannotMatch of p * e

let rec bindings (p:p) (e:e) : e SMap.t =
  match p,e with
  | P_unit,E_const Unit -> SMap.empty
  | P_unit,_ -> SMap.singleton (gensym ()) e
  | P_var x,e -> SMap.singleton x e
  | P_tuple ps,E_tuple es ->
    if (List.compare_lengths ps es <> 0) then raise (CannotMatch (p,e)) else
    List.fold_left2 (fun m p v -> bindings p v ++ m) SMap.empty ps es
  | P_tuple ps,e when static e ->
      let n = List.length ps in
      let rs = List.mapi (fun i p -> bindings p (E_app (E_const (Op (GetTuple {pos=i;arity=n})),e))) ps in
      List.fold_right (++) rs  SMap.empty
  | _ -> raise (CannotMatch (p,e))


(** [pat2exp p] converts the pattern [p] into an expression *)
let rec pat2exp = function
| P_unit -> E_const Unit
| P_var x -> E_var x
| P_tuple ps -> E_tuple (List.map pat2exp ps)

(** [exp2pat e] converts the expression [e] into a pattern *)
let rec exp2pat = function
| E_const Unit -> P_unit
| E_var x -> P_var x
| E_tuple ps -> P_tuple (List.map exp2pat ps)
| _ -> assert false (* todo : syntax error ! *)

