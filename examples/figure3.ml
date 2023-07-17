(**

  This example (Figure 3 in the paper) is a stateful instantaneous function
  running internaly a non-instantaneous function

  Command to compile:
  ------------------

    ./mixc examples/figure3.ml

*)


let collatz(x) =
  let rec loop(n,t) =
    if n = 1 then t else
    if n mod 2 = 0 then
      loop(n/2,t+1)
    else loop(3*n+1,t+1)
  in loop(x,1) ;;

let aro(a,reset) =
  let step(x) =
    (x or a) & not reset
  in reg step last false ;;


let main(m,n,threshold,reset) =
  let (t1,rdy1) = exec collatz(m) default 0 in
  let (t2,rdy2) = exec collatz(n) default 0 in
  let a = (rdy1 & (t1 > threshold)) or
  (rdy2 & (t2 > threshold)) in
  aro(a,reset) ;;
