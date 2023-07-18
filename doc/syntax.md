Each source program is a suite of definitions following the grammar above.
A function `main` must be defined among these definitions.


```
pi :=                                (* program *)
      d                              (* declaration *)
    | d pi                           (* declarations *)

d ::=                                (* declaration *)
      let x = v                      (* value *)
    | let rec x = v                  (* tail-recursive value *)
    | let static x = c^n             (* static array declaration *)

v ::=                                (* value *)
      c                              (* constant *)
    | fun p -> e                     (* local function *)
    | fix f (fun p -> e)             (* tail-recursive local function *)
    | (v, ... v)                     (* tuple *)

e ::=                                (* expression *)
      c                              (* constant *)
    | x                              (* variable *)
    | if e then e else e             (* conditional *)
    | let p = e in e                 (* let-binding *)
    | (e, ... e)                     (* tuple *)
    | fun p -> e                     (* local function *)
    | fix f (fun p -> e)             (* tail-recursive local function *)
    | e e                            (* application *)
    | (e : ty)                       (* type annotation *)
    | e || e                         (* parallel composition *)
    | reg x last e                   (* register update *)
    | exec x default e               (* asynchronous execution *)
    | x[e]                           (* static array access *)
    | x.length                       (* static array length *)
    | x[e] <- e                      (* static array assignment *)

p ::=                                (* pattern *)
      ()                             (* unit *)
    | _                              (* wildcard *)
    | x                              (* variable *)
    | (p, ... p)                     (* tuple *)
    | (p : ty)                       (* type annotation *)

c ::=                                (* constant *)
      0 | 1 ...                      (* integer *)
    | true | false                   (* boolean *)
    | op                             (* operator *)
    | ext                            (* external function *)
    | (c : ty)                       (* type annotation *)

op ::=                               (* operator *)
      + | - | * | / | mod
    | < | > | <= | >= | == | <>
    | & | or | not | abs
    | wait | to_string
    | assert | print | random

ext ::=                              (* external function *)
        array_make                   (* dynamic allocation of an array *)
      | array_get                    (*  array access *)
      | array_length                 (*  array length *)
      | array_set                    (*  array assignment *)

ty ::=                               (* type *)
       'a                            (* type variable *)
     | int | int<sz>                 (* integer *)
     | bool                          (* boolean *)
     | ty => ty                      (* instantaneous function *)
     | ty -> ty                      (* non-instantaneous function *)
     | (ty, ... ty)                  (* tuple *)
     | ty static<sz>                 (* static array *)
     | ty array                      (* dynamic array *)

sz ::=                               (* type size *)
        0 | 1 | ... n                (* literal size *)
     | sz + sz                       (* addition *)
     | max(sz,sz)                    (* maaximum *)
```

There is also some syntactic sugar:

```
let x p : ty = e1 in e2          (* equivalent to [let x = fun p -> (e1 : ty) in e2]         *)
let rec x p : ty = e1 in e2      (* equivalent to [let x = fix x (fun p -> (e1 : ty)) in e2] *)  
let x p : ty = e1 in e2          (* equivalent to [let x = fun p -> (e1 : ty) in e2]         *)
let rec x p : ty = e1 in e2      (* equivalent to [let x = fix x (fun p -> (e1 : ty)) in e2] *)
```


## Informal semantics

All constructs react instantaneously except tail-recursive calls (that pause until the next clock tick) and call to asynchronous primitives like `array_make` (that take several instant).

## Functional features
- Functions can take functions as parameters but cannot return functions. Partial application is not supported.
- Tuples cannot contain functions (except for argument passing)
- Only tail-recursion is supported.
