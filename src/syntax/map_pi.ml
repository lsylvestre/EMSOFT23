open Ast

let map (f : e -> e) (pi: pi) : pi =
  let ds = List.map (fun (x,e) -> x,f e) pi.ds in
  let main = f pi.main in
  { pi with ds ; main }

let for_all (f : e -> bool) (pi: pi) : bool =
  List.for_all (fun (_,e) -> f e) pi.ds && f pi.main
