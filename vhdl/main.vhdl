library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.runtime.all;


entity main is
  
  port(signal clk    : in std_logic;
       signal reset  : in std_logic;
       signal run    : in std_logic;
       signal rdy    : out value(0 to 0);
       signal argument : in value(0 to 31);
       signal result : out value(0 to 31));
       
end entity;
architecture rtl of main is

  type \t_state\ is (Compute, \loop\);
  signal state: \t_state\;
  type t_loop is (I1, I2);
  signal loop_id : t_loop;
  signal loop_arg : value(0 to 63);
  
  begin
    process(reset, clk)
      variable \$19\, \$23\, \$22\, \$16\, \$20\, \$18\, \$21\, \$17\
               : value(0 to 63);
      variable \$1\, \$4\
               : value(0 to 0);
      variable \$5\, y, \$24\, \$15\, n, \$7\, \$11\, \$8\, \$10\, \$2\, t
               : value(0 to 31);
      
    begin
      if (reset = '1') then
        
        loop_arg <= (others => '0');
        default_zero(t); default_zero(\$2\); default_zero(\$17\); 
        default_zero(\$10\); default_zero(\$21\); default_zero(\$18\); 
        default_zero(\$20\); default_zero(\$8\); default_zero(\$11\); 
        default_zero(\$7\); default_zero(\$16\); default_zero(\$4\); 
        result <= (others => '0');
        default_zero(n); default_zero(\$22\); default_zero(\$15\); 
        default_zero(\$24\); default_zero(y); default_zero(\$1\); 
        default_zero(\$23\); default_zero(\$19\); default_zero(\$5\); 
        rdy <= "1";
        state <= Compute;
        
      elsif rising_edge(clk) then
        if run = '1' then
          case state is
          when \loop\ =>
            n := loop_arg(0 to 31);
            t := loop_arg(32 to 63);
            \$23\ := n & "0000000000000000000000000000" & X"1";
            \$1\ := mixc_eq(\$23\);
            if \$1\(0) = '1' then
              \$15\ := t;
              case loop_id is
              when I1 =>
                \$24\ := \$15\;
                result <= \$24\;
                rdy <= "1";
                state <= Compute;
              when I2 =>
                y := \$15\;
                loop_arg <= y & "0000000000000000000000000000" & X"1";
                loop_id <= I1;
                state <= \loop\;
              end case;
            else
              \$22\ := n & "0000000000000000000000000000" & X"2";
              \$2\ := mixc_mod(\$22\);
              \$21\ := \$2\ & "0000000000000000000000000000" & X"0";
              \$4\ := mixc_eq(\$21\);
              if \$4\(0) = '1' then
                \$17\ := n & "0000000000000000000000000000" & X"2";
                \$10\ := mixc_div(\$17\);
                \$16\ := t & "0000000000000000000000000000" & X"1";
                \$11\ := mixc_add(\$16\);
                loop_arg <= \$10\ & \$11\;
                state <= \loop\;
              else
                \$20\ := "0000000000000000000000000000" & X"3" & n;
                \$5\ := mixc_mult(\$20\);
                \$19\ := \$5\ & "0000000000000000000000000000" & X"1";
                \$7\ := mixc_add(\$19\);
                \$18\ := t & "0000000000000000000000000000" & X"1";
                \$8\ := mixc_add(\$18\);
                loop_arg <= \$7\ & \$8\;
                state <= \loop\;
              end if;
            end if;
          when Compute =>
            rdy <= "0";
            loop_arg <= argument & "0000000000000000000000000000" & X"1";
            loop_id <= I2;
            state <= \loop\;
          end case;
        end if;
    end if;
  end process;
end architecture;
