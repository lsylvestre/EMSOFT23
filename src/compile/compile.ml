open Fsm_syntax
open Fsm_comp

module D = Display_internal_steps

let print_elaborated_code_flag = ref true


let compile name ty fmt pi =

  D.display_pi D.Front pi;

  let pi = Encode_reg_exec.encode_pi pi in

  D.display_pi D.Encode pi;

  let pi = Middle_end.compile pi in
  let pi = Fun_shape_entry_point.fun_shape_entry_point pi in
  let pi = Rename_main_arg.rename_main_arg_pi pi in

  D.display_pi D.MiddleEnd pi;

  let (result,infos,fsm) as design = Fsm_comp.compile ~result:"result" pi in
  let statics = List.map (function x,Ast.Static_array(c,n) -> x,Fsm_syntax.Static_array(Fsm_comp.to_c c,n)) pi.statics in

  Display_target.(display Fsm fsm);

  let fsm = Flat_transitions.flatten fsm in

  let fsm = Flat_let_atom.flat_let_atom fsm in
  Display_target.(display Flat fsm);

  let typing_env = Fsm_typing.typing_circuit ~statics ty (result,fsm) in

  let name = "main" in
  let state_var = "state" in
  let argument = "argument" in
  let compute = "Compute" in
  let rdy = "rdy" in

  let fsm = Encode.encode_all ~result:(Delayed,result) ~compute ~state_var ~rdy:(Some(Delayed,rdy)) fsm in
  Display_target.(display Encode fsm);

  let (argument,result) = Gen_vhdl.pp_component fmt ~name ~state_var ~argument ~result ~compute ~rdy ~statics typing_env infos fsm in
  (argument,result,typing_env)
