
-- AVALON MM-slave wrapper around the core main IP
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
entity avs_main is
  port (avs_s0_address : in std_logic_vector(7 downto 0)  := (others => '0');
          -- 00000000  : control/status register (b1=start, b0=rdy)
          
        avs_s0_read        : in  std_logic                     := '0';
        avs_s0_readdata    : out std_logic_vector(31 downto 0);
        avs_s0_write       : in  std_logic                     := '0';
        avs_s0_writedata   : in  std_logic_vector(31 downto 0) := (others => '0');
        clock_clk          : in  std_logic                     := '0';
        reset_reset        : in  std_logic                     := '0');
end entity;

architecture rtl of avs_main is
  component main is
    port (signal clk : in std_logic;
          signal reset : in std_logic;
          signal run : in std_logic;
          signal rdy : out std_logic;
          signal argument : in std_logic_vector(31 downto 0);
          signal result : out std_logic_vector(31 downto 0));
  end component;
  
  signal result: std_logic_vector(31 downto 0);
  signal argument: std_logic_vector(31 downto 0);
  signal rdy: std_logic;
  type write_state_t is (Idle, StartAsserted);
  signal write_state: write_state_t;
begin
  main_CC : component main
    port map (clk => clock_clk,
              reset => reset_reset,
              run => '1',
              rdy => rdy,
              argument => argument,
              result => result
              );
  
  WRITE: process (clock_clk, reset_reset)
  begin
    if reset_reset = '1' then
      write_state <= Idle;
    elsif rising_edge(clock_clk) then
      case write_state is
        when StartAsserted =>
          -- start <= '0';
          write_state <= Idle;
        when Idle =>
          if avs_s0_write = '1' then
            case avs_s0_address is
              when "00000000" => -- writing CSR asserts start for one clock period
                -- start <= '1';
                write_state <= StartAsserted;
              when others => NULL;
            end case;
          end if;
        end case;
      end if;
    end process;
  READ: process (clock_clk)
  begin
    if rising_edge(clock_clk) then
      if avs_s0_read = '1' then
        case avs_s0_address is
          when "00000000" => avs_s0_readdata <= X"0000000" & "000" & rdy;
                             -- when reading CSR, bit 0 is rdy
          when others => null;
          end case;
        end if;
      end if;
    end process;
  end architecture;
