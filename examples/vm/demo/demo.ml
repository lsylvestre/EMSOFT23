open CustomStdlib ;;

let a = (5,(4,3),(2,1))

let one = (let (_,_,(_,v)) = a in v)

let rec fact n =
  if n < 2 then one else n * fact (n - one)

exception E of int

let add3_plus_1 x y z = raise (E (x + y + z + one))

let f = add3_plus_1 2

let g = f 1 ;;

let v6 = try g 2 with E n -> n ;;

let v = Some [|v6|] ;;

print_int (match v with
	         | Some v -> fact (Array.get v 0) 
	         | None -> 0) ;;
