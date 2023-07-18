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

  type \t_state\ is (Compute);
  signal state: \t_state\;
  
  begin
    process(reset, clk)
      variable \$5\, \$41\, \$34\, \$37\, \$43\, \$40\, \$42\, \$35\, \$39\, 
               \$44\, \$36\, \$18\, \$38\, \$21\, \$25\, \$45\, \$29\
               : value(0 to 1);
      variable \$8_r\, \$19\, \$23\, \$33\, x, \$23_init\, \$6_a\, \_\, 
               \$13\, \$22\, \$21_init\, \$9\, \$19_init\, \$18_init\, c, 
               \$7_b\, \$14\, \$16\, \$7\, \$11\, \$20\, \$10\, \$22_init\, 
               t, \$20_init\
               : value(0 to 0);
      
    begin
      if (reset = '1') then
        default_zero(\$29\); default_zero(\$45\); default_zero(\$20_init\); 
        default_zero(t); default_zero(\$22_init\); default_zero(\$25\); 
        default_zero(\$21\); default_zero(\$10\); default_zero(\$38\); 
        default_zero(\$18\); default_zero(\$20\); default_zero(\$36\); 
        default_zero(\$11\); default_zero(\$7\); default_zero(\$44\); 
        default_zero(\$16\); default_zero(\$39\); default_zero(\$14\); 
        default_zero(\$7_b\); default_zero(c); 
        result <= (others => '0');
        default_zero(\$18_init\); default_zero(\$19_init\); 
        default_zero(\$35\); default_zero(\$9\); default_zero(\$21_init\); 
        default_zero(\$22\); default_zero(\$42\); default_zero(\$13\); 
        default_zero(\$40\); default_zero(\_\); default_zero(\$43\); 
        default_zero(\$37\); default_zero(\$6_a\); default_zero(\$34\); 
        default_zero(\$41\); default_zero(\$23_init\); default_zero(x); 
        default_zero(\$33\); default_zero(\$23\); default_zero(\$19\); 
        default_zero(\$5\); default_zero(\$8_r\); 
        rdy <= "1";
        state <= Compute;
        
      elsif rising_edge(clk) then
        if run = '1' then
          case state is
          when Compute =>
            rdy <= "0";
            \$6_a\ := argument(0 to 0);
            \$7_b\ := argument(1 to 1);
            c := argument(2 to 2);
            \$8_r\ := argument(3 to 3);
            if mixc_not(\$23_init\)(0) = '1' then
              \$23\ := "0";
              \$23_init\ := "1";
            end if;
            \$45\ := \$23\ & \$6_a\;
            \$10\ := mixc_or(\$45\);
            \$11\ := mixc_not(\$8_r\);
            \$44\ := \$10\ & \$11\;
            \$9\ := mixc_and(\$44\);
            \$23\ := \$9\;
            \$13\ := \$23\;
            if mixc_not(\$22_init\)(0) = '1' then
              \$22\ := "0";
              \$22_init\ := "1";
            end if;
            \$43\ := \$22\ & \$7_b\;
            \$10\ := mixc_or(\$43\);
            \$11\ := mixc_not(\$8_r\);
            \$42\ := \$10\ & \$11\;
            \$9\ := mixc_and(\$42\);
            \$22\ := \$9\;
            \$14\ := \$22\;
            \$41\ := \$13\ & \$14\;
            \$16\ := mixc_and(\$41\);
            if mixc_not(\$21_init\)(0) = '1' then
              \$21\ := "0" & \$16\;
              \$21_init\ := "1";
            end if;
            \$5\ := \$16\ & \$21\(0 to 0);
            \$21\ := \$5\;
            \$29\ := \$21\;
            \_\ := \$29\(0 to 0);
            x := \$29\(1 to 1);
            \$7\ := mixc_not(x);
            \$40\ := \$7\ & \$16\;
            t := mixc_and(\$40\);
            if mixc_not(\$20_init\)(0) = '1' then
              \$20\ := "0";
              \$20_init\ := "1";
            end if;
            \$39\ := \$20\ & t;
            \$10\ := mixc_or(\$39\);
            \$11\ := mixc_not(\$8_r\);
            \$38\ := \$10\ & \$11\;
            \$9\ := mixc_and(\$38\);
            \$20\ := \$9\;
            \$13\ := \$20\;
            if mixc_not(\$19_init\)(0) = '1' then
              \$19\ := "0";
              \$19_init\ := "1";
            end if;
            \$37\ := \$19\ & c;
            \$10\ := mixc_or(\$37\);
            \$11\ := mixc_not(\$8_r\);
            \$36\ := \$10\ & \$11\;
            \$9\ := mixc_and(\$36\);
            \$19\ := \$9\;
            \$14\ := \$19\;
            \$35\ := \$13\ & \$14\;
            \$16\ := mixc_and(\$35\);
            if mixc_not(\$18_init\)(0) = '1' then
              \$18\ := "0" & \$16\;
              \$18_init\ := "1";
            end if;
            \$5\ := \$16\ & \$18\(0 to 0);
            \$18\ := \$5\;
            \$25\ := \$18\;
            \_\ := \$25\(0 to 0);
            x := \$25\(1 to 1);
            \$7\ := mixc_not(x);
            \$34\ := \$7\ & \$16\;
            \$33\ := mixc_and(\$34\);
            result <= \$33\;
            rdy <= "1";
            state <= Compute;
          end case;
        end if;
    end if;
  end process;
end architecture;
