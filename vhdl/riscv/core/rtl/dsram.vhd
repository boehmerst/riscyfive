library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity dsram is 
  generic (
    width_g : positive := 32;
    size_g  : positive := 8
  );
  port (
    dat_o   : out std_ulogic_vector(width_g - 1 downto 0);
    adr_i   : in  std_ulogic_vector(size_g - 1 downto 0);
    ena_i   : in  std_ulogic;
    dat_w_i : in  std_ulogic_vector(width_g - 1 downto 0);
    adr_w_i : in  std_ulogic_vector(size_g - 1 downto 0);
    wre_i   : in  std_ulogic;
    clk_i   : in  std_ulogic
  );
end entity dsram;

architecture beh of dsram is

  type ram_t is array(2**size_g-1 downto 0) of std_ulogic_vector(width_g - 1 downto 0);
  
  signal ram :  ram_t;

begin
  -----------------------------------------------------------------------------
  -- memory
  -----------------------------------------------------------------------------
  mem0: process(clk_i)
  begin
    if rising_edge(clk_i) then
      if ena_i = '1' then
        if wre_i = '1' then
          ram(to_integer(unsigned(adr_w_i))) <= dat_w_i;
        end if;
        dat_o <= ram(to_integer(unsigned(adr_i)));
      end if;
    end if;
  end process mem0;

end architecture beh;

