open Format

let size_ty_opt = function
| None -> 1
| Some t -> Gen_vhdl.size_ty t

let gen_testbench fmt typing_env name ty ((argument,ta),(result,tr)) (args_for_simul: _ list) =
  let argument_size = size_ty_opt ta in
  let result_size = size_ty_opt tr in
  fprintf fmt "@[<v>
  library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.runtime.all;
  use work.all;

  entity tb_%s is@," name;
  fprintf fmt "end entity;

  architecture tb of tb_%s is
  " name;
  fprintf fmt "component main
    port(
      signal clk    : in std_logic;
      signal reset  : in std_logic;
      signal run    : in std_logic;
      signal rdy    : out value(0 to 0);
      signal %s : in value(0 to %d);
      signal result : out value(0 to %d)" argument (argument_size-1) (result_size-1);

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt ";@,signal caml_heap_base : in value(0 to 31)";
  end;

  fprintf fmt ");@,end component;";

  fprintf fmt "
  signal tb_run: std_logic;
  signal tb_argument: std_logic_vector(0 to %d);
  signal tb_result: std_logic_vector(0 to %d);
  signal tb_rdy: value(0 to 0);
  signal tb_clk: std_logic;
  signal rst: std_logic;" (argument_size-1) (result_size-1);

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt "@,signal tb_caml_heap_base : value(0 to 31);";
  end;

  fprintf fmt "
  begin

  RESET: process
  begin
    rst <= '1';
    wait for 2 ns;
    rst <= '0';
    wait;
  end process;


  CLOCK: process
  begin
    tb_clk <= '1';
    wait for 5 ns;
    tb_clk <= '0';
    wait for 5 ns;
  end process;

  U1: main port map(tb_clk,rst,tb_run,tb_rdy,tb_argument,tb_result";

  begin
    if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
      fprintf fmt ",tb_caml_heap_base";
  end;
  fprintf fmt ");";
  fprintf fmt "
  process
  begin
    tb_run <= '0';
    wait for 20 ns;
    tb_run <= '1';
      -- Start computation@,";

  fprintf fmt "  tb_argument <= %s;@," (Gen_vhdl.default_zero_value argument_size);

  List.iter (fun arg_for_simul ->
    let t = Fsm_typing.typing_a typing_env arg_for_simul in
    Option.iter (fun ta -> Fsm_typing.unify t ta) ta;
  fprintf fmt "  tb_argument <= %a;@," Gen_vhdl.pp_a arg_for_simul;
    fprintf fmt "
    wait for 10 ns;
    while tb_rdy = \"0\" loop -- not equivalent to wait until
      wait for 10 ns;
    end loop;@,";
    (* assert false report "v15=" & std_logic'image(v15(0)) severity note; *)
  ) args_for_simul;

  fprintf fmt "
    wait;
  end process;

  end architecture;@,@]
  ";
  Format.pp_force_newline fmt ()
