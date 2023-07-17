(**

  This example is a classical synchronous program.

  Command to compile:
  ------------------

    ./mixc examples/abcro.ml -main abcro -arg "(false,false,false,false);(true,false,false,false);(false,true,true,false);(false,false,false,false);(true,true,true,false);(true,true,true,true);(true,true,true,false)"

*)



(** [fby(a,b)] implements a delay as a one place buffer
    initialized with [a] and filled with [b] at the end of each clock tick *)
let fby(a,b) =
  let step(x,y) = (b,x) in
  let (_,x) = reg step last (a,b) in x ;;

(** [edge(i)] detects rising edges on input [i] *)
let edge (i:bool) : bool =
  not (fby(false,i)) & i ;;

(** [aro(i,reset)] sustains value true as soon as
    input [i] is true until [reset] is false *)
let aro (i,reset) : bool =
  let step(s) = (s or i) & not reset in
  reg step last false ;;

(** classical Esterel program ABRO:
    "wait [a] and [b] then emit [o], reset with [r]"
 *)
let abro (a,b,r) : bool =
  edge(aro(a,r) & aro(b,r)) ;;

(** [abcro] adds an input [c] to [abro],
    illustrating compositionality within the language *)
let abcro (a,b,c,r) : bool =
  let t = abro(a,b,r) in
  abro(t,c,r) ;;
