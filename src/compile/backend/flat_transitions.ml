open Combinatorial
open Fsm_syntax

let rec flat s =
  match s with
  | S_skip
  | S_continue _ 
  | S_set _ 
  | S_setptr _
  | S_setptr_write _
  | S_buffer_set _
  | S_call _ -> 
      (* no sub-instruction *) [],s
  | S_if(a,s1,so) ->
    let bs1,s1' = flat s1 in
    (match so with
    | Some s2 ->
        let bs2,s2' = flat s2 in
        bs1@bs2, S_if(a,s1',Some s2')
    | None ->
        bs1, S_if(a,s1',None))
  | S_case(a,hs, so) ->
      let bss,hs' = List.split @@ List.map (fun (qi,si) -> 
                                            let (bsi,si') = flat si in (bsi,(qi,si'))) hs in
      let bs = List.concat bss in
      (match so with
      | Some s2 ->
          let bs2,s2' = flat s2 in
          bs@bs2, S_case(a,hs', Some s2')
      | None ->
          bs, S_case(a,hs', None))
  | S_seq(s1,s2) ->
      let bs1,s1' = flat s1 in
      let bs2,s2' = flat s2 in
      bs1@bs2,S_seq(s1',s2')
  | S_letIn(x,a,s1) ->
      let bs1,s1' = flat s1 in
      bs1,S_letIn(x,a,s1')
  | S_fsm(id,rdy,result,compute,ts,s1,b) ->
      let bs1,s1' = flat s1 in
      let bss,ts' = List.split @@ List.map (fun (qi,si) -> 
                                              let (bsi,si') = flat si in (bsi,(qi,si'))) ts in
      let bs = List.concat bss in
      [],S_fsm(id,rdy,result,compute,bs1@bs@ts',s1',b)

  | S_let_transitions(ts,s1) ->
      let bs1,s1' = flat s1 in
      let bss,ts' = List.split @@ List.map (fun (qi,si) -> 
                                              let (bsi,si') = flat si in (bsi,(qi,si'))) ts in
      let bs = List.concat bss in
      bs1@bs@ts',s1'

let flatten (ts,s) =
  let bs1,s' = flat s in
  let bss,ts' = List.split @@ List.map (fun (qi,si) -> 
                                          let (bsi,si') = flat si in (bsi,(qi,si'))) ts in
  let bs = List.concat bss in
  bs1@bs@ts', s'
