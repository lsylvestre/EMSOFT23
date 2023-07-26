
library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package runtime is
    alias value is std_logic_vector;
    constant mixc_true : value(0 to 0) := "1";
    constant mixc_false : value(0 to 0)  := "0";
    constant mixc_unit : value(0 to 0)  := "1";
    function mixc_add  (arg: value)  return value;
    function mixc_sub  (arg: value)  return value;
    function mixc_mult (arg: value)  return value;
    function mixc_div  (arg: value)  return value;
    function mixc_mod  (arg: value)  return value;
    function mixc_eq   (arg: value)  return value;
    function mixc_neq  (arg: value)  return value;
    function mixc_lt   (arg: value)  return value;
    function mixc_gt   (arg: value)  return value;
    function mixc_le   (arg: value)  return value;
    function mixc_ge   (arg: value)  return value;
    function mixc_if   (arg : value) return value;
    function mixc_and  (arg : value) return value;
    function mixc_or   (arg : value) return value;
    function mixc_xor  (arg : value) return value;
    function mixc_not  (arg : value) return value;
    function mixc_id   (arg : value) return value;
    function mixc_lor  (arg : value) return value;
    function mixc_land (arg : value) return value;
    function mixc_lxor (arg : value) return value;
    function mixc_lsl (arg: value) return value;
    function mixc_lsr (arg: value) return value;
    function mixc_asr (arg: value) return value;
    -- -- todo add mixc_asr
    function of_string (s: string)   return value; 
    function to_string (a: std_logic_vector) return string;

    function integer_of_value(arg: value) return integer; 
    function mixc_compute_address(caml_heap_base:value;a:value) return value;
    
    function misc_string_length(arg:value) return value;
    function misc_resize(arg:value; k:integer) return value;

    procedure default_zero (xvar: out value);
    procedure misc_print(arg:value);
    procedure misc_print_string(arg:value);
    procedure misc_print_int(arg:value);
    procedure misc_print_newline(arg:value);
end package;

package body runtime is
  function mixc_add (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := signed(arg(0 to length-1)) + signed(arg(length to arg'length - 1));
      return value(r);
    end;

  function mixc_sub (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := signed(arg(0 to length-1)) - signed(arg(length to arg'length - 1)); 
      return value(r);
    end;

  function mixc_mult (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) * signed(arg(length to arg'length - 1)),length);
      return value(r);
    end;

  function mixc_div (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) / signed(arg(length to arg'length - 1)),length);
      return value(r);
    end;

  function mixc_mod (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      r := resize(signed(arg(0 to length-1)) mod signed(arg(length to arg'length - 1)),length);
      return value(r);
    end;


  function mixc_eq (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) = signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

    function mixc_neq (arg: value) return value is
      constant length: natural := arg'length / 2;
      variable r : value (0 to 0);
      begin
        if signed(arg(0 to length-1)) = signed(arg(length to arg'length - 1)) then
          r := "0";
        else
          r:= "1";
        end if;
        return r;
      end;

  function mixc_lt (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) < signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_gt (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) > signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_le (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) <= signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_ge (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : value (0 to 0);
    begin
      if signed(arg(0 to length-1)) >= signed(arg(length to arg'length - 1)) then
        r := "1";
      else
        r:= "0";
      end if;
      return r;
    end;

  function mixc_if(arg : value) return value is
    constant length: natural := (arg'length - 1) / 2;
    variable r : value (0 to length-1);
    begin 
      if arg(0) = '1' then
         r := arg(1 to length);
      else 
         r := arg(length + 1 to length * 2);
      end if;
      return r;
    end;

  function mixc_and(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' and arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function mixc_or(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' or arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function mixc_xor(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if (arg(0) = '1' xor arg(1) = '1') then
        r := "1";
      else
        r := "0";
      end if;
      return r;
    end;

  function mixc_not(arg : value) return value is
    variable r : value (0 to 0);
    begin 
      if arg(0) = '1' then
        r := "0";
      else
        r := "1";
      end if;
      return r;
    end;

  function mixc_lor (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      for i in 0 to length-1 loop
        r(i) := arg(i) or arg(i+length);
      end loop;
      return value(r);
    end;

  function mixc_land (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      for i in 0 to length-1 loop
        r(i) := arg(i) and arg(i+length);
      end loop;
      return value(r);
    end;
  
  function mixc_lxor (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : signed (0 to length - 1);
    begin
      for i in 0 to length-1 loop
        r(i) := arg(i) xor arg(i+length);
      end loop;
      return value(r);
    end;

  function mixc_lsl (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : unsigned (0 to length - 1);
    begin
      r := unsigned(arg(0 to length-1)) sll integer_of_value(arg(length to arg'length - 1));
      return value(r);
    end;

  function mixc_lsr (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : unsigned (0 to length - 1);
    begin
      r := unsigned(arg(0 to length-1)) srl integer_of_value(arg(length to arg'length - 1));
      return value(r);
    end;

  function mixc_asr (arg: value) return value is
    constant length: natural := arg'length / 2;
    variable r : unsigned (0 to length - 1);
    variable n : integer range 0 to length - 1;
    variable sign : std_logic;
    begin
      n := integer_of_value(arg(length to arg'length - 1));
      r := unsigned(arg(0 to length-1)) srl n;
      sign := arg(0);
      for i in 0 to n loop -- not tested yet
        r(i) := sign;
      end loop;
      return value(r);
    end;

  function mixc_id(arg : value) return value is
    begin 
      return arg;
    end;

  function of_string(s: string) return value is 
        constant ss: string(1 to s'length) := s; 
        variable answer: std_logic_vector(0 to 8 * s'length - 1); 
        variable p: integer; 
        variable c: integer; 
    begin 
        for i in ss'range loop
            p := 8 * i;
            c := character'pos(ss(i));
            answer(p - 8 to p-1) := std_logic_vector(to_unsigned(c,8)); 
        end loop; 
        return answer;
    end function; 

  function to_string (a: std_logic_vector) return string is
    variable b : string (1 to a'length) := (others => NUL);
    variable stri : integer := 1; 
    begin
        for i in a'range loop
            b(stri) := std_logic'image(a((i)))(2);
        stri := stri+1;
        end loop;
    return b;
    end function;

    function integer_of_value(arg: value) return integer is
      variable r : unsigned (0 to arg'length - 1);
    begin 
       r := unsigned(arg);
       return to_integer(r);
    end function;

    function mixc_compute_address(caml_heap_base:value;a:value) return value is
      begin
        return value(signed(caml_heap_base) + signed(a(0 to 31)) + (signed(a(32 to 63)) * 4));
      end function;

    function misc_string_length(arg:value) return value is
      begin
        return std_logic_vector(to_signed(arg'length / 8,16));
      end;

    function misc_resize(arg:value; k:integer) return value is
      begin
         return std_logic_vector(resize(unsigned(arg),k));
      end;

    procedure default_zero (xvar: out value) is
    begin
      for i in 0 to xvar'length - 1 loop
        xvar(0) := '0';
      end loop;
    end procedure;

    procedure echo (arg : in string) is
    begin
      std.textio.write(std.textio.output, arg);
    end procedure echo;

    procedure misc_print(arg:value) is
      begin
        echo(to_string(arg));
      end procedure;
    
    function char_of_value(arg : value(0 to 7)) return character is
    -- from https://groups.google.com/g/comp.lang.vhdl/c/_kI2ZwxVVVk
    constant xmap :integer :=0;
    variable TEMP :integer :=0;
    begin
      for i in arg'range loop
        temp:=temp*2;
        case arg(i) is
          when '0' | 'L' => null;
          when '1' | 'H' => temp :=temp+1;
          when others => temp :=temp+xmap;
        end case;
      end loop;
      return character'val(temp);
    end function;

    procedure misc_print_string(arg:value) is
      variable s : string (0 to arg'length / 8);
      begin
        for i in 0 to s'length-2 loop
            s(i) := char_of_value(arg(i*8 to i*8+7));
        end loop;
        echo(s);
      end procedure;

    procedure misc_print_int(arg:value) is
      begin
         echo(integer'image(to_integer(signed(arg))));
      end procedure;

    procedure misc_print_newline(arg:value) is
      begin
          echo(" " &LF);
      end procedure;

end runtime;
