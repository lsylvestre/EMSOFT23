(**

  This example from the paper illustrates the sharing of tail-recursive call.

  Command to compile:
  ------------------

    ./mixc examples/sharing.ml -arg "42" -relax


  Option -relax allows to compile a non-instantaneous program (this example is not reactive !).

*)

let collatz(x) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then
      loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(x,1) ;;

let main(x) =
  let y = collatz(x) in
  collatz(y) ;;
