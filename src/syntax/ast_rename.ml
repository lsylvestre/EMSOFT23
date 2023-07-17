open Ast
open Ast_subst

module Gensym = struct
  let make_name n x =
    let x = if x = "_" then "w" else x in
    "S0"^string_of_int n^"_"^x


  let c = ref 0

  let gensym x =
    incr c;
    if String.length x > 2 && x.[0] = 'S' && x.[1] = '0' then
      (match String.index_from_opt x 2 '_' with
      | Some i -> make_name !c (String.sub x (i+1) (String.length x - i-1))
      | None -> make_name !c x)
    else make_name !c x

  let reset () = c := 0

end

open Gensym

(** [rename_pat p] rename all names in the pattern [p],
  assuming that any variable is bound several times in p *)
let rec rename_pat p =
   match p with
   | P_unit -> P_unit
   | P_var x -> P_var (gensym x)
   | P_tuple ps -> P_tuple (List.map rename_pat ps)



let rec rename_e e =
  let ss = rename_e in
  match e with
  | E_deco(e1,deco) ->
      E_deco(ss e1, deco)
  | E_const _ ->
      e
  | E_var _ -> e
  | E_fix(f,(p,e1)) ->
      let g = gensym f in
      let pz = rename_pat p in
      let e1_ren = subst_e f (E_var g) @@
                   subst_p_e p (pat2exp pz) e1 in
      E_fix (g,(pz,ss e1_ren))
  | E_fun(p,e1) ->
      let pz = rename_pat p in
      E_fun (pz,ss (subst_p_e p (pat2exp pz) e1))
  | E_app(e1,e2) ->
      E_app(ss e1,ss e2)
  | E_reg(V ev,e0) ->
      E_reg(V (ss ev), ss e0)
  | E_exec(e1,e2,k) ->
      (* [k] is in a different name space than variables *)
      E_exec(ss e1,ss e2,k)
  | E_tuple(es) ->
      E_tuple(List.map ss es)
  | E_letIn(p,e1,e2) ->
      let pz = rename_pat p in
      let ez = pat2exp pz in
      E_letIn(pz, ss e1, ss @@ subst_p_e p ez e2)
  | E_if(e1,e2,e3) ->
      E_if(ss e1, ss e2, ss e3)
  | E_lastIn(x,e1,e2) ->
      let y = gensym x in
      E_lastIn(y,ss e1,ss @@ subst_e x (E_var y) e2)
  | E_set(x,e1) ->
      E_set(x,ss e1)
  | E_static_array_get(x,e1) ->
      E_static_array_get(x,ss e1)
  | E_static_array_length _ ->
      e
  | E_static_array_set(x,e1,e2) ->
      E_static_array_set(x,ss e1,ss e2)
  | E_step(e1,k) ->
      (* [k] is in a different name space than variables *)
      E_step(ss e1,k)
  | E_par(e1,e2) ->
      E_par(ss e1, ss e2)

let rename_pi pi =
  let ds = List.map (fun (x,e) -> x,rename_e e) pi.ds in
  let main = rename_e pi.main in
  { pi with ds ; main }
