
(**

  This example illustrates the use of a global static array.
  Larger arrays can be implemented using RAM blocks (onchip-memory).

  Command to compile:
  ------------------

    ./mixc examples/static.ml -relax


  Option -relax allows to compile a non-instantaneous program (this example is not reactive !).

*)


let static a = (42:16)^10 ;;

let rec loop (s,i) =
  if i < a.length
  then loop ((a[i] + s), i+1)
  else s ;;

let main () =
  exec loop (0,0) default 0 ;;
