
type ty =                (** type *)
  | T_const of tconst    (** type constant *)
  | T_var of tvar ref    (** type variable *)
  | T_tuple of ty list   (** type constructor for tuples *)
  | T_fun of {
      arg:ty ;  (** functional type constructor annotated with a response time [dur] *)
      dur:ty ;  (* [arg -(dur)-> ret] is the type of a function which, given a value of *)
      ret:ty    (* type [arg], produces a value of type [ret] after no more than [dur] clock ticks *)
    }
  | T_array of ty (* dynamic array of elements of type [ty] *)
  | T_string of ty (** string parameterized by its size using a the size type [ty] *)
  | T_static of {
      elem : ty ; (** static array of elements of type [elem], *)
      size : ty   (** parameterized by its size using a the size type [size] *)
    }
  
  (* sized types for check response time and static datastructures *)
  | T_size of int      (** n *)
  | T_infinity         (** plus infinite *)
  | T_add of ty * ty   (** t + t'  *)
  | T_max of ty * ty   (** max(t,t') *)
  | T_le of ty * ty    (** t such that t <= t' *)

and tconst = (** type constant *)
| TBool      (** boolean type [bool] *)
| TInt of ty (** integer type [int<ty>]
                 where [ty] is a size type denoting the size of the interger. 
                 All integers are signed. *)
| TUnit      (** unit type [unit] *)

and tvar =              (** type variable *)
  | Unknown of int      (** type unknown 'a identified by a unique integer *)
  | Ty of ty            (* instantiated type variable *)


type deco = Prelude.loc (** information of position from the source file *)

type x = string         (** identifier [x] *)

type l = string         (** location (i.e., pointer) [l] *)

type c =                (** constant [c] *)
  | Unit                (** unit value [()] *)
  | Bool of bool        (** boolean [true | false] *)
  | Int of int * ty     (** integer [n] of given size *)
  | String of string    (** string [s] *)
  | Op of op            (** primitive [op] *)
  | External of extern  (** asynchronous primitive *)
  | V_loc of l          (** pointer [l], only for the interpreter, not in source programs *)

and op = (** primitives *)
       (* instantaneous primitives *)
         Add | Sub | Mult | Div | Mod (* signed operations *)
       | Lt | Le | Gt | Ge | Eq | Neq
       | And | Or | Xor | Not | Abs
       | Land | Lor | Lxor | Lsl | Lsr | Asr
       | GetTuple of {
            pos : int ;   (* indice of the projection to access *)
            arity : int   (* size (i.e. number of projections) of the tuple *)
         }
        (* instantaneous primitives *)
       | Wait of int 
       | TyConstr of ty
         (* for simulation *)
       | Assert | String_length | To_string | Print | Random

(** asynchronous primitives manipulating data structures in shared memoy *)
and extern =
  | Array_make   (** dynamically allocate an array *)
  | Array_set    (** modify a dynamic array *)
  | Array_get    (** read one element in a dynamic array *)
  | Array_length (** read the size of a a dynamic array *)

type p =                     (** pattern [p] *) 
    P_unit                   (** constant unit [()] *)
  | P_var of x               (** variable [x] *)
  | P_tuple of p list        (** tuple [(p1, ... pn)] *)

type e =                      (** expression     [e]                     *)
    E_deco of e * deco        (** annot an expression with its location  in the source code *)
  | E_const of c              (** constant       [c]                     *)
  | E_var of x                (** variable       [x,y,f,g ...]           *)
  | E_app of e * e            (** application    [e1 e2]                 *)
  | E_tuple of e list         (** tuple          [e1, ... en]            *)
  | E_letIn of p * e * e      (** let-bindings   [let p = e1 in e2]      *)
  | E_if of e * e * e         (** conditional    [if e1 then e2 else e3] *)
  | E_match of e * (c * e) list * e (** switch/case    [match e with | c -> e | ... | _ -> e] *)
  | E_fun of p * e            (** function       [fun p -> e]            *)
  | E_fix of x * (p * e)      (** recursive function [fix (fun p -> e)]  *)
  | E_reg of v * e            (** register       [reg f last e]          *)
  | E_exec of e * e * x       (** exec           [(exec e default e)^x]  *)
  | E_static_array_get of x * e     (** static array access     [x[e]]      *)
  | E_static_array_length of x      (** static array length access x.length *)
  | E_static_array_set of x * e * e (** static array assignment [x[e] <- e] *)
  | E_par of e * e            (** parallel       [e1 || e2]              *)

    (* the following constructs are used internally *)
  | E_lastIn of x * e * e     (** local variable [var x = e in e]        *)
  | E_set of x * e            (** assignment     [x <- e]
                                  type checking must ensure that [x]
                                  is bound using the var/in construct    *)
  | E_step of e * x           (** spawn          [(step e)^x]            *)

and v = V of e

type static =                 (* static toplevel data *)
  | Static_array of c * int   (** static global array [c^n] *)

(** each program is a sequence of toplevel definitions (static arrays 
    and functions) coupled with an entry point, e.g. the variable [main] 
    referting to one of those definitions *)
type pi = {
  statics : (x * static) list ; (** static global arrays *)
  ds : (x * e) list ;           (** toplevel function definitions *)
  main : e                      (* entry point *)
}


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

(** symbol generator *)

let gensym : ?prefix:string -> unit -> x =
  let c = ref 0 in
  (fun ?(prefix="$v") () ->
    incr c; prefix^string_of_int !c)

(** associative array having key of type [string] *)
module SMap = Map.Make(String)

(** type of these associative arrays *)
type 'a smap = 'a SMap.t
type 'a env = 'a SMap.t

(** [m1 + m2] merges the bindings from [m1] and [m2]. In case of conflict, 
   the bindings from [m2] is keeped. *)
let (++) (m1 : 'a smap) (m2 : 'a smap)  : 'a smap =
  SMap.union (fun _ _ v2 -> Some v2) m1 m2

(** [smap_of_list l] constructs an associative array from the associative list [l] *)
let smap_of_list (l : (x * 'a) list) : 'a smap =
  List.fold_right (fun (x,v) m -> SMap.add x v m) l SMap.empty

(** [vars_of_p p] returns the (free) variables used in the pattern [p] *)
let rec vars_of_p (p:p) : unit smap =
  match p with 
  | P_unit -> SMap.empty
  | P_var x -> SMap.singleton x ()
  | P_tuple ps ->
    List.fold_left (fun m p -> vars_of_p p ++ m) SMap.empty ps

(** [un_annot e] removes decoration (i.e., position in the source file) 
   around expression [e] *)
let rec un_deco (e:e) : e =
  match e with
  | E_deco(e,_) -> e
  | e -> e

(** [un_annot e] removes both decorations
   and type constraints around expression [e] *)
let rec un_annot (e:e) : e =
  match e with
  | E_deco(e,_) -> un_annot e
  | E_app(E_const(Op(TyConstr _)),e) -> un_annot e
  | e -> e

(** [is_constant e] returns [true] iff [e] is a constant *)
let is_constant (e:e) : bool =
  match un_annot e with
  | E_const _ -> true
  | _ -> false

(** [is_variable e] returns [true] iff [e] is a variable *)
let is_variable (e:e) : bool =
  match un_annot e with
  | E_var _ -> true
  | _ -> false

(** [as_variable e] returns [x] if [e] is a variable [x].
    Otherwise, raise [Invalid_argument "as_variable"] *)
let as_variable (e:e) : x =
  match un_annot e with
  | E_var x -> x
  | _ -> invalid_arg "as_variable"

(** [evaluated e] returns [true] iff [e] is a value *)
let rec evaluated (e:e) : bool =
  match un_annot e with
  | E_const _ | E_fun _ | E_fix _ -> true
  | E_tuple es -> List.for_all evaluated es
  | E_app(E_const(Op(TyConstr _)),e) -> evaluated e
  | _ -> false

(** [loc_of e] returns the location arround [e] if it exists, 
   or a default location *)
let rec loc_of (e:e) : Prelude.loc =
  match e with
  | E_deco(_,loc) ->
      loc
  | e -> Prelude.dloc

(** [mk_loc loc e] plugs the expression [e] into a node 
   indicating the location [loc] *)
let mk_loc (loc: Prelude.loc) (e:e) : e =
  E_deco(e,loc)

(** [ty_annot ~ty e] plugs the expression [e] under a type constraints 
   such as [(e:ty)] *)
let ty_annot ~(ty:ty) (e:e) : e =
  E_app(E_const (Op (TyConstr ty)),e)

(** like [ty_annot] with an optional argument [~ty] *)
let ty_annot_opt ~(ty:ty option) (e:e) : e =
  match ty with
  | None -> e
  | Some ty ->ty_annot ~ty e

