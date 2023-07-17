(**

  This example (Figure 2 in the paper) is a stateful instantaneous function
  which returns true as soon as input a is true until reset is true

  Command to compile:
  ------------------

    ./mixc examples/figure2.ml -main aro -arg "(false,false);(true,false);(false,false);(false,false);(false,true);(true,true);(true,false);(false,false)"

*)

let aro(a,reset) =
  let step(x) =
    (x or a) & not reset
  in reg step last false ;;
