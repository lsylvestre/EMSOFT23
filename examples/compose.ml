(** This is a simple example of higher-order function. 
  
  Command to compile:
  ------------------

  ./mixc examples/compose.ml -arg "1;2;3;4;5" 

*)

let compose(f,g,x) = f (g(x)) ;;
  
let add (x,y) = x + y ;;
let mul x = x * 2 ;;

let main x =
  let inc z = add(z,1) in
  compose(inc,mul,x) ;;