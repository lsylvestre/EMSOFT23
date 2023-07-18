open Ast
open Pattern

(**  
    globalize all functions, by Lambda-lifting [Johnsson, 1982]:

    Given an expression [e] in ANF form with renaming (i.e., all bindings have different name), 
    return a program [(ds,e)] in ANF-form with renaming
    where all functions are defined in [ds].

    Our version of the algorithmic works in two steps:
    - [lifting] adds free variables of each function definition, as a tuple,
      into the second parameter (environment) of the function definition
      and all these call. [lift] has to be repeat until all functions are close
    - [globalize] globalizes all (close) functions.
      The order of globalized bindings is important
      since global functions are *not* mutually-recursive.
**)

(** [fv ~statics ~decls e] returns the free variables of [e] that are not 
    global definitions (bound in ~static and ~decls) *)
let fv ~statics ~decls e =
  SMap.filter (fun x _ -> not (SMap.mem x statics) && not (List.mem_assoc x decls)) (Free_vars.fv e)

(** [has_changed] boolean flag setted to true each time a [lift] pass modifies
    the input expression *)
let has_changed = ref false

(* bind each function name to its lexical environment
    (as a collection of name grouped in a pattern) *)
type env = p smap

(** [lifting ~statics ~decls e] lifts expression [e]
    considering [~statics] and [~decls] as toplevel definitions
    that should not be added to lexical environments. *)
let lifting ~statics ~decls (env:env) (e:e) : e =
  let rec lift env e =
    let open Ast in
    match e with
    | E_deco _ ->
        Ast_undecorated.still_decorated e
    | E_var _ ->
        e
    | E_const _ ->
        e
    | E_fun _ | E_fix _ ->
        e
    | E_app(E_const _,xc2) ->
        assert (Anf.is_xc xc2);
        e
    | E_app(E_var f,e1) ->
        (match SMap.find_opt f env with
         | None | Some (P_unit) -> E_app(E_var f,e1)
         | Some p ->
            E_app(E_var f,E_tuple[e1; pat2exp p]))
    | E_app _ -> assert false
    | E_if(exc1,e2,e3) ->
        assert (Anf.is_xc exc1);
        E_if(exc1, lift env e2, lift env e3)
    | E_match(e1,hs,e_els) ->
      assert(Anf.is_xc e1);
      E_match(e1,List.map (fun (c,e) -> c,lift env e) hs,lift env e_els)
    | E_letIn(P_var f,(E_fun(p,e1) as phi),e2) ->
        let e1' = lift env e1 in
        let xs = fv ~statics ~decls phi in
        let vp = (vars_of_p p) in
        let p_env' = xs |> SMap.filter (fun x _ -> not (SMap.mem x vp) && not (SMap.mem x env))
                        |> SMap.bindings
                        |> List.map (fun (x,_) -> P_var x)
                        |> group_ps in
        if not (SMap.is_empty (vars_of_p p_env')) then
          (has_changed := true;
           let env2, ef = (SMap.add f p_env' env, E_fun(P_tuple[p;p_env'],e1')) in
           E_letIn(P_var f,ef,lift env2 e2) )
        else
           let env2 = SMap.add f p_env' env in
           E_letIn(P_var f,(E_fun(p,e1')),lift env2 e2)
    | E_letIn(P_var f,(E_fix(g,(p,e1)) as phi),e2) ->
        if f <> g then let e1' = Ast_subst.subst_e g (E_var f) e1 in
                        let e2' = Ast_subst.subst_e g (E_var f) e2 in
                        lift env (E_letIn(P_var f,(E_fix(f,(p,e1'))),e2'))
        else
        let e1' = lift env e1 in
        let xs = fv ~statics ~decls phi in
        let vp = (vars_of_p p) in
        let p_env' = xs |> SMap.filter (fun x _ -> not (SMap.mem x vp) && not (SMap.mem x env) && x <> f)
                        |> SMap.bindings
                        |> List.map (fun (x,_) -> P_var x)
                        |> group_ps in
        if not (SMap.is_empty (vars_of_p p_env')) then (has_changed := true;
          let env2,ef = (SMap.add f p_env' env, E_fix(f,(P_tuple[p;p_env'],e1'))) in
          E_letIn(P_var f,ef,lift env2 e2) )
         else
          (let env2 = SMap.add f p_env' env in
           E_letIn(P_var f,(E_fix(f,(p,e1'))),lift env2 e2) )
    | E_letIn(p,e1,e2) ->
        E_letIn(p,lift env e1,lift env e2)
    | E_tuple es_atoms ->
        E_tuple es_atoms
    | E_lastIn(x,e1,e2) ->
        E_lastIn(x,lift env e1,lift env e2)
    | E_set(x,exc1) ->
        assert (Anf.is_xc exc1);
        e
    | E_static_array_get(x,e1) ->
        assert(Anf.is_xc e1);
        e
    | E_static_array_length _ ->
        e
    | E_static_array_set(x,e1,e2) ->
        assert(Anf.is_xc e1);
        assert(Anf.is_xc e2);
        e
    | E_step(e1,k) ->
        E_step(lift env e1,k)
         | E_par _ -> e (* do not transform sub-expressions under step and // *)
    | E_reg _ | E_exec _ ->
        assert false (* already expanded *)
  in lift env e

(** lifting has to be perform several time until reaching a fixpoint,
   then [has_changed] remains false *)
let rec lift_until ~statics ~decls (e:e) =
  has_changed := false;
  let e' = lifting ~statics ~decls SMap.empty e in
  if !has_changed then lift_until ~statics ~decls e' else e'



(** [globalize e] globalizes all local *close* functions in expression [e] *)
let globalize (e:e) : ((x * e) list * e) =
  let rec glob e =
    let open Ast in
    match e with
    | E_deco _ ->
        Ast_undecorated.still_decorated e
    | E_const _ | E_var _ | E_tuple _ | E_app _ ->
        assert (Anf.in_anf e);
        [],e
    | E_letIn(P_var f,(E_fix _ | E_fun _ as v),e2) ->
        let dsv,v = glob v in
        let ds,e2' = glob e2 in
        (dsv@[(f,v)]@ds),e2'
    | E_fix(f,(p,e1)) ->
        let ds1,e1' = glob e1 in
        ds1,E_fix(f,(p,e1'))
    | E_fun(p,e1) ->
        let ds1,e1' = glob e1 in
        ds1,E_fun(p,e1')
    | E_if(xc1,e2,e3) ->
        assert (Anf.is_xc xc1); (* assume ANF *)
        let ds2,e2' = glob e2 in
        let ds3,e3' = glob e3 in
        ds2@ds3,E_if(xc1,e2',e3') (* ds2 and ds3 disjoint *)
    | E_match(e1,hs,e_els) ->
      assert(Anf.is_xc e1);
      let dss,hs' = List.split @@ List.map (fun (c,e) -> let ds,e' = glob e in ds,(c,e')) hs in
      let ds,e_els' = glob e_els in
      List.concat dss@ds, E_match(e1,hs',e_els')
    | E_letIn(p,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_letIn(p,e1',e2')
    | E_lastIn(x,e1,e2) ->
        let ds1,e1' = glob e1 in
        let ds2,e2' = glob e2 in
        ds1@ds2,E_lastIn(x,e1',e2')
    | E_set(x,e1) ->
        assert(Anf.is_xc e1);
        [],e
    | E_static_array_get(x,e1) ->
        assert(Anf.is_xc e1);
        [],e
    | E_static_array_length _ ->
        [],e
    | E_static_array_set(x,e1,e2) ->
        assert(Anf.is_xc e1);
        assert(Anf.is_xc e2);
        [],e
    | E_step _ | E_par _ -> 
        (* do not transform sub-expressions under step and // *)
        [],e
    | E_reg _ | E_exec _ ->
        assert false (* already expanded *)
  in glob e


(** [lambda_lifting ~statics ~decls e] lambda-lifts expression [e],
    considering [~statics] and [~decls] as toplevel definitions
    that should not be added to lexical environments. *)
let lambda_lifting ~statics ~decls (e:e) : ((x * e) list * e) =
    let e_lifted = (lift_until ~statics ~decls e) in
    let ds',e_globalized = globalize e_lifted in
    (ds',e_globalized)


(** [lambda_lifting_pi pi] lambda-lifts program [pi]. *)
let lambda_lifting_pi (pi:pi) : pi =
  let statics = smap_of_list pi.statics 
  in
  let rec loop acc ds_to_lambda_lift =
    match ds_to_lambda_lift with
    | [] ->
        let (ds,e) = lambda_lifting ~statics ~decls:acc pi.main in
        {pi with ds = List.rev_append acc ds ; main = e }
    | (x,e)::ds_to_lambda_lift' ->
            let (ds,e') = lambda_lifting ~statics ~decls:acc e in
            loop (List.rev_append (ds@[(x,e')]) acc) ds_to_lambda_lift'
  in loop [] pi.ds

