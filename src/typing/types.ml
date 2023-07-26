
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



let tint tz = T_const (TInt tz)
let tbool = T_const TBool
let tunit = T_const TUnit
let fun_ty t1 n t2 =
  T_fun { arg = t1;
          dur = n;
          ret = t2 }

(* instantaneous function type *)
let (==>) t1 t2 =
  fun_ty t1 (T_size 0) t2

let unknown =
  let c = ref 0 in
  fun () ->
    let ty = T_var (ref (Unknown (!c))) in
    incr c; ty