open Format
open Fsm_syntax

open Gen_hw_tcl


let flag_heap_onchip = ref false
let flag_stack_onchip = ref false


(* modified at each compilation *)
let size_for_coding_arguments_id = ref 8

let rec log2 n =
  if n <= 1 then 0 else 1 + log2 (n asr 1)

let set_nb_arguments_max_per_circuit n =
  size_for_coding_arguments_id := log2 n + 1


let bin_of_int d =
  let pad = !size_for_coding_arguments_id in
  assert (d >= 0 && pad > 1 && log2 d < !size_for_coding_arguments_id);
  let open Bytes in
  let b = make pad '0' in
  let rec aux d i =
    if d >= 0 && i >= 0 then
      (set b i (Char.chr ((d land 1) + Char.code '0'));
       aux (d lsr 1) (i-1))
    else ()
  in
  aux d (pad-1);
  to_string b

(* ***************************************** *)

let flag_print_compute_time = ref false
let flag_print_compute_time_short = ref false

(* ***************************************** *)


let set_result dst fmt x =
  fprintf fmt "%s <= %s" dst x

let gen_cc fmt envi envo name =

  fprintf fmt "@[<v>-- AVALON MM-slave wrapper around the core %s IP@," name;
  fprintf fmt "library IEEE;@,";
  fprintf fmt "use IEEE.std_logic_1164.all;@,";
  fprintf fmt "use IEEE.numeric_std.all;@,";
  fprintf fmt "@[<v 2>entity avs_%s is@," name;
  fprintf fmt "port (@[< v>";
  fprintf fmt "@[<v 2>avs_s0_address : in std_logic_vector(%d downto 0)  := (others => '0');@," (!size_for_coding_arguments_id - 1);
  fprintf fmt "-- %s  : control/status register (b1=start, b0=rdy)@," (bin_of_int 0);
  let regs = envi@envo in
  let len = List.length regs in
  List.iteri (fun i (x,t) ->
    fprintf fmt "-- %s  : %s register@," (bin_of_int (i+1)) x) regs;

  (if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "-- %s  : caml_heap_base register@," (bin_of_int (len+1)));

  fprintf fmt "@]@,avs_s0_read        : in  std_logic                     := '0';@,";
  fprintf fmt "avs_s0_readdata    : out std_logic_vector(31 downto 0);@,";
  fprintf fmt "avs_s0_write       : in  std_logic                     := '0';@,";
  fprintf fmt "avs_s0_writedata   : in  std_logic_vector(31 downto 0) := (others => '0');@,";
  fprintf fmt "clock_clk          : in  std_logic                     := '0';@,";
  fprintf fmt "reset_reset        : in  std_logic                     := '0'";

  if !allow_heap_access then begin
    fprintf fmt ";@,@,-- READ MASTER INTERFACE@,";
    fprintf fmt "avm_rm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_rm_read      : out std_logic;@,";
    fprintf fmt "avm_rm_readdata  : in std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_rm_waitrequest : in std_logic@,";
  end;

  if !allow_heap_assign then begin
    fprintf fmt ";@,@,-- WRITE MASTER INTERFACE@,";
    fprintf fmt "avm_wm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_wm_write      : out std_logic;@,";
    fprintf fmt "avm_wm_writedata  : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "avm_wm_waitrequest : in std_logic@,";
  end;

  fprintf fmt ");@]@]@,";
  fprintf fmt "end entity;@,@,";
  fprintf fmt "@[<v 2>architecture rtl of avs_%s is@," name;
  fprintf fmt "@[<v 2>component %s is@," name;
  fprintf fmt "port (@[< v>";
  fprintf fmt "signal clk : in std_logic;@,";
  fprintf fmt "signal reset : in std_logic;@,";
  fprintf fmt "signal run : in std_logic;@,";
  fprintf fmt "signal rdy : out std_logic;@,";
  fprintf fmt "signal argument : in std_logic_vector(31 downto 0);@,";
  fprintf fmt "signal result : out std_logic_vector(31 downto 0)";

  if !allow_heap_access || !allow_heap_assign then begin
    fprintf fmt ";@,signal caml_heap_base   : in std_logic_vector(31 downto 0)";
  end;

  if !allow_heap_access then begin
    fprintf fmt ";@,signal avm_rm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_rm_read      : out std_logic;@,";
    fprintf fmt "signal avm_rm_readdata  : in std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_rm_waitrequest : in std_logic";
  end;

  if !allow_heap_assign then begin
    fprintf fmt ";@,signal avm_wm_address   : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_wm_write      : out std_logic;@,";
    fprintf fmt "signal avm_wm_writedata  : out std_logic_vector(31 downto 0);@,";
    fprintf fmt "signal avm_wm_waitrequest : in std_logic";
  end;

  fprintf fmt ");@]@]@,";
  fprintf fmt "end component;@,@,";

  fprintf fmt "signal result: std_logic_vector(31 downto 0);@,";
  fprintf fmt "signal argument: std_logic_vector(31 downto 0);@,";
  fprintf fmt "signal rdy: std_logic;@,";
  fprintf fmt "type write_state_t is (Idle, StartAsserted);@,";
  fprintf fmt "signal write_state: write_state_t;@]@,";

  if !allow_heap_access || !allow_heap_assign then
    fprintf fmt "signal caml_heap_base : std_logic_vector(31 downto 0);@,";

  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>%s_CC : component %s@," name name;
  fprintf fmt "port map (@[< v>";
  fprintf fmt "clk => clock_clk,@,";
  fprintf fmt "reset => reset_reset,@,";
  fprintf fmt "run => '1',@,";
  fprintf fmt "rdy => rdy,@,";
  fprintf fmt "argument => argument,@,";
  fprintf fmt "result => result@,";

  fprintf fmt "@]);@]@,@,";
  fprintf fmt "WRITE: process (clock_clk, reset_reset)@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>if reset_reset = '1' then@,";
  fprintf fmt "write_state <= Idle;@]@,";
  fprintf fmt "@[<v 2>elsif rising_edge(clock_clk) then@,";
  fprintf fmt "@[<v 2>case write_state is@,";
  fprintf fmt "@[<v 2>when StartAsserted =>@,";
  fprintf fmt "-- start <= '0';@,";
  fprintf fmt "write_state <= Idle;@]@,";
  fprintf fmt "@[<v 2>when Idle =>@,";
  fprintf fmt "@[<v 2>if avs_s0_write = '1' then@,";
  fprintf fmt "@[<v 2>case avs_s0_address is@,";
  fprintf fmt "@[<v 2>when \"%s\" => -- writing CSR asserts start for one clock period@," (bin_of_int 0);
  fprintf fmt "-- start <= '1';@,";
  fprintf fmt "write_state <= StartAsserted;@]@,";

  (if !Fsm_comp.allow_heap_access || !Fsm_comp.allow_heap_assign then
    fprintf fmt "@[<v 2>when \"%s\" => caml_heap_base <= avs_s0_writedata;@]@,"
             (bin_of_int (len+1)));

  fprintf fmt "when others => NULL;@]@,";
  fprintf fmt "end case;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end case;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end process;@]@,";
  fprintf fmt "READ: process (clock_clk)@,";
  fprintf fmt "@[<v 2>begin@,";
  fprintf fmt "@[<v 2>if rising_edge(clock_clk) then@,";
  fprintf fmt "@[<v 2>if avs_s0_read = '1' then@,";
  fprintf fmt "@[<v 2>case avs_s0_address is@,";
  fprintf fmt "when \"%s\" => @[<v>avs_s0_readdata <= X\"0000000\" & \"000\" & rdy;@," (bin_of_int 0);
  fprintf fmt "-- when reading CSR, bit 0 is rdy@]@,";

  List.iteri (fun i (x,_) ->
   fprintf fmt "@[<v 2>when \"%s\" => %a;@]@,"
       (bin_of_int (i+1)) (set_result "avs_s0_readdata") x) (envi@envo);

  fprintf fmt "when others => null;@,";
  fprintf fmt "end case;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end if;@]@,";
  fprintf fmt "end process;@]@,";
  fprintf fmt "end architecture;@]@,"


let gen_glue_code ?labels ?xs_opt () =
  let open Filename in
  let (^^) = Filename.concat in
  let open Format in

  let name = "main" in

  let dst = "vhdl" in
  let o2b_dir = dst ^^ "o2b" in
  let qsys_dir = o2b_dir ^^ "qsys" in
  let cc_name           = o2b_dir ^^ (name ^"_cc.vhd")
  and hw_tcl_name       = qsys_dir ^^ (name ^ "_cc_hw.tcl")
  and ext_tcl_name      = qsys_dir ^^ (name ^ "_cc_ext.tcl")
  and platform_tcl_name = qsys_dir ^^ "platform.tcl"
  
  in
  let fmt = std_formatter in
  let  cc_oc = open_out cc_name
  and hw_tcl_oc = open_out hw_tcl_name
  and ext_tcl_oc = open_out ext_tcl_name
  and platform_tcl_oc = open_out platform_tcl_name
  in

  set_formatter_out_channel cc_oc;
  gen_cc fmt [] [] name;
  pp_print_flush fmt ();

  set_formatter_out_channel hw_tcl_oc;
  Gen_hw_tcl.mk_hw_tcl name fmt !size_for_coding_arguments_id;
  pp_print_flush fmt ();

  set_formatter_out_channel ext_tcl_oc;
  Gen_hw_tcl.mk_ext_gen_qsys name fmt;
  pp_print_flush fmt ();

  set_formatter_out_channel platform_tcl_oc;
  Gen_platform_tcl.gen_platform_tcl fmt;
  pp_print_flush fmt ();

  close_out cc_oc;
  close_out hw_tcl_oc;
  close_out ext_tcl_oc;
  close_out platform_tcl_oc ;

  let bsp_update_script_oc = open_out "vhdl/o2b/bsp/bsp_update.tcl" in
  set_formatter_out_channel bsp_update_script_oc;
  Gen_bsp_update_tcl.gen_bsp_update_tcl 
    ~heap:!flag_heap_onchip
    ~stack:!flag_stack_onchip fmt;
  pp_print_flush fmt ();
  close_out bsp_update_script_oc;

  Printf.printf "  info: circuit  \"%s\"  generated in folder vhdl/.\n" name
    
