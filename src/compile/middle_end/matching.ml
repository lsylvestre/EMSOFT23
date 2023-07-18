open Ast
open Ast_subst


(** compile pattern matching from an ANF expression       (* TODO: in fact no: it is not in ANF form after propagation *)
    to an equivalent ANF expression.

   e.g. an expression of the form [let (x,y) = e in e']
        becomes an expression of the form
          [let z = e' in
           let x = fst z in
           let y = snd z in e''] where z is fresh.

  After this transformation, source programs are as follows:

  ;; expression
  e ::= 
        xc
      | fun x -> e                   ;; <-- no pattern [p] any more
      | fix f (fun x -> e)           ;; <-- no pattern [p] any more
      | xc xc
      | (px,px ... px)
      | if xc then e else e
      | match xc with c -> e | ... | _ -> e
      | let x = e in e               ;; <-- no pattern [p] any more
      | reg x last xc
      | (exec e xc)^id
      | var x = e in e
      | x <- xs
      | (step e)^k
      | x[xc]
      | x.length
      | x[xc] <- xc

  This corresponds to ANF-form with let bindings of the form [let x = e in e]
  rather than [let p = e in e].

*)

(* custom heuristic to check if an expression can be propagated.
   not to be confused with [Combinatorial.combinatorial] *)
let rec combinatorial = function
| E_deco (e,_) -> combinatorial e
| E_var _ -> true
| E_const c -> true
| E_if(e1,e2,e3) ->
    combinatorial e1 && combinatorial e2 && combinatorial e3
| E_match(e1,hs,e_els) ->
    combinatorial e1 && List.for_all (fun (_,e) -> combinatorial e) hs && combinatorial e_els
| E_app(E_var _,e1)
| E_app(E_fix _,e1) -> false
| E_app(E_const(Op op),e2) ->
    Combinatorial.op_combinatorial op && combinatorial e2
| E_app(e1,e2) ->
    false
| E_tuple es ->
    List.for_all combinatorial es
| E_letIn _ | E_fun _ | E_fix _ ->
    false
| E_reg _ | E_exec _ ->
    false
| E_lastIn (_, e1, e2) ->
    combinatorial e1 && combinatorial e2
| E_static_array_length _ ->
    true
| E_static_array_get _ | E_static_array_set _ ->
    false
| E_set (_, _) | E_step (_, _) ->
    false
| E_par (e1,e2) ->
    combinatorial e1 && combinatorial e2



(** [is_projection e] returns [true] if e is a projection [px]. *)
let rec is_projection = function
| E_var _ -> true
| E_app(E_const (Op (GetTuple {pos=_;arity=_})),e) -> is_projection e
| _ -> false

(** [projection e i size] make the AST accessing projection [i]
    of the [size]-tuple [e] *)
let projection e i size =
  match e with
  | E_tuple es when List.for_all combinatorial es ->
      (* optimisation needed for eliminating functional values in tuples *)
      List.nth es i
  | _ -> E_app(E_const (Op (GetTuple {pos=i;arity=size})),e)

(** [matching e] translate an ANF expression [e] in an ANF expression where all
   let-bindings are of the form [let x = e1 in e2]. *)
let rec matching e =
  match e with
  | E_deco _ ->
      Ast_undecorated.still_decorated e
  | E_var _ | E_const _ ->
       e (* no-subexpressions *)
  | E_tuple(es) ->
      E_tuple(List.map matching es)
  | E_app(e1,px) ->
         E_app(matching e1,px)
  | E_if(px,e1,e2) ->
      E_if(px,matching e1,matching e2)
    | E_match(px,hs,e_els) ->
      E_match(px,List.map (fun (c,e) -> c,matching e) hs,matching e_els)
  | E_letIn(P_unit,E_var _,e2) ->
      matching e2
  | E_letIn(P_var z,e1,e2) ->
     E_letIn(P_var z,matching e1,matching e2)
  | E_letIn(p,e1,e2) ->
      if not (is_projection e1) then
        let z = gensym () in
        E_letIn(P_var z,matching e1,matching @@ E_letIn(p,E_var z, e2))
      else (match p with
            | P_var z -> (match matching (*?*) e1 with
                          | E_var _ -> matching @@ subst_e z e1 e2
                          | _ -> E_letIn(p,e1,matching e2))
            | P_unit ->
                let z = Ast.gensym () in
                E_letIn(P_var z,matching e1,matching e2)
            | P_tuple(ps) ->
               matching @@
               let size = List.length ps in
               let bs = List.mapi (fun i p ->
                            p, projection e1 i size) ps in
                List.fold_right (fun (p,e) acc -> E_letIn(p,e,acc)) bs e2)
  | E_fun(P_var x,e) ->
      E_fun(P_var x,matching e)
  | E_fun(p,e) ->
      let x = gensym () in
      matching @@ E_fun(P_var x,E_letIn(p,E_var x,e))
  | E_fix(f,(P_var x,e)) ->
      E_fix(f,(P_var x,matching e))
   | E_fix(f,(p,e)) ->
      let x = gensym () in
      matching @@ E_fix(f,(P_var x,E_letIn(p,E_var x,e)))
  | E_lastIn(x,e1,e2) ->
      E_lastIn(x,matching e1,matching e2)
  | E_set(x,e1) ->
      E_set(x,matching e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,matching e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,matching e1,matching e2)
  | E_step _ | E_par _ -> e (* do not transform sub-expressions under step and // constructs *)
  | E_reg _ | E_exec _ ->
       assert false (* already expanded *)

let matching_pi pi =
  Map_pi.map matching pi
