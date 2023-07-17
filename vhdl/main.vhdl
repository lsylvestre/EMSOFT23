library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 3);
       signal result : out value(0 to 0));
       
end entity;
architecture rtl of main is

  type t_state is (Compute);
  signal state: t_state;
  
  begin
    process(reset, clk)
      variable S024_t : value(0 to 0);
      variable v20 : value(0 to 0);
      variable v45 : value(0 to 1);
      variable S05_x : value(0 to 0);
      variable v21 : value(0 to 1);
      variable v44 : value(0 to 1);
      variable v36 : value(0 to 1);
      variable v38 : value(0 to 1);
      variable v18_init : value(0 to 0);
      variable v22 : value(0 to 0);
      variable v23 : value(0 to 0);
      variable v39 : value(0 to 1);
      variable v20_init : value(0 to 0);
      variable v22_init : value(0 to 0);
      variable S020_a : value(0 to 0);
      variable vanf14 : value(0 to 0);
      variable vanf5 : value(0 to 1);
      variable vanf13 : value(0 to 0);
      variable vanf16 : value(0 to 0);
      variable S023_r : value(0 to 0);
      variable vanf9 : value(0 to 0);
      variable S021_b : value(0 to 0);
      variable vanf10 : value(0 to 0);
      variable v35 : value(0 to 1);
      variable v37 : value(0 to 1);
      variable v19 : value(0 to 0);
      variable v33 : value(0 to 0);
      variable v43 : value(0 to 1);
      variable S04_w : value(0 to 0);
      variable v18 : value(0 to 1);
      variable S022_c : value(0 to 0);
      variable v25 : value(0 to 1);
      variable vanf11 : value(0 to 0);
      variable v40 : value(0 to 1);
      variable v29 : value(0 to 1);
      variable v19_init : value(0 to 0);
      variable v41 : value(0 to 1);
      variable vanf7 : value(0 to 0);
      variable v21_init : value(0 to 0);
      variable v34 : value(0 to 1);
      variable v42 : value(0 to 1);
      variable v23_init : value(0 to 0);
      
    begin
      if (reset = '1') then
        S024_t := (others => '0');
        v20 := (others => '0');
        v45 := (others => '0');
        S05_x := (others => '0');
        v21 := (others => '0');
        v44 := (others => '0');
        v36 := (others => '0');
        v38 := (others => '0');
        v18_init := (others => '0');
        v22 := (others => '0');
        v23 := (others => '0');
        v39 := (others => '0');
        v20_init := (others => '0');
        v22_init := (others => '0');
        S020_a := (others => '0');
        vanf14 := (others => '0');
        vanf5 := (others => '0');
        vanf13 := (others => '0');
        result <= (others => '0');
        vanf16 := (others => '0');
        S023_r := (others => '0');
        vanf9 := (others => '0');
        S021_b := (others => '0');
        vanf10 := (others => '0');
        v35 := (others => '0');
        v37 := (others => '0');
        v19 := (others => '0');
        v33 := (others => '0');
        v43 := (others => '0');
        S04_w := (others => '0');
        v18 := (others => '0');
        S022_c := (others => '0');
        v25 := (others => '0');
        vanf11 := (others => '0');
        v40 := (others => '0');
        v29 := (others => '0');
        v19_init := (others => '0');
        v41 := (others => '0');
        vanf7 := (others => '0');
        v21_init := (others => '0');
        v34 := (others => '0');
        v42 := (others => '0');
        v23_init := (others => '0');
        rdy <= "1";
        state <= Compute;
        
      elsif rising_edge(clk) then
        if run = '1' then
          case state is
          when Compute =>
            rdy <= "0";
            S020_a := argument(0 to 0);
            S021_b := argument(1 to 1);
            S022_c := argument(2 to 2);
            S023_r := argument(3 to 3);
            if mixc_not(v23_init)(0) = '1' then
              v23 := "0";
              v23_init := "1";
            end if;
            v45 := v23 & S020_a;
            vanf10 := mixc_or(v45);
            vanf11 := mixc_not(S023_r);
            v44 := vanf10 & vanf11;
            vanf9 := mixc_and(v44);
            v23 := vanf9;
            vanf13 := v23;
            if mixc_not(v22_init)(0) = '1' then
              v22 := "0";
              v22_init := "1";
            end if;
            v43 := v22 & S021_b;
            vanf10 := mixc_or(v43);
            vanf11 := mixc_not(S023_r);
            v42 := vanf10 & vanf11;
            vanf9 := mixc_and(v42);
            v22 := vanf9;
            vanf14 := v22;
            v41 := vanf13 & vanf14;
            vanf16 := mixc_and(v41);
            if mixc_not(v21_init)(0) = '1' then
              v21 := "0" & vanf16;
              v21_init := "1";
            end if;
            vanf5 := vanf16 & v21(0 to 0);
            v21 := vanf5;
            v29 := v21;
            S04_w := v29(0 to 0);
            S05_x := v29(1 to 1);
            vanf7 := mixc_not(S05_x);
            v40 := vanf7 & vanf16;
            S024_t := mixc_and(v40);
            if mixc_not(v20_init)(0) = '1' then
              v20 := "0";
              v20_init := "1";
            end if;
            v39 := v20 & S024_t;
            vanf10 := mixc_or(v39);
            vanf11 := mixc_not(S023_r);
            v38 := vanf10 & vanf11;
            vanf9 := mixc_and(v38);
            v20 := vanf9;
            vanf13 := v20;
            if mixc_not(v19_init)(0) = '1' then
              v19 := "0";
              v19_init := "1";
            end if;
            v37 := v19 & S022_c;
            vanf10 := mixc_or(v37);
            vanf11 := mixc_not(S023_r);
            v36 := vanf10 & vanf11;
            vanf9 := mixc_and(v36);
            v19 := vanf9;
            vanf14 := v19;
            v35 := vanf13 & vanf14;
            vanf16 := mixc_and(v35);
            if mixc_not(v18_init)(0) = '1' then
              v18 := "0" & vanf16;
              v18_init := "1";
            end if;
            vanf5 := vanf16 & v18(0 to 0);
            v18 := vanf5;
            v25 := v18;
            S04_w := v25(0 to 0);
            S05_x := v25(1 to 1);
            vanf7 := mixc_not(S05_x);
            v34 := vanf7 & vanf16;
            v33 := mixc_and(v34);
            result <= v33;
            rdy <= "1";
            state <= Compute;
          end case;
        end if;
    end if;
  end process;
end architecture;
