library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.core_pkg.all;

entity gprf is
  generic (
    dmem_width_g : positive := 32;
    gprf_size_g  : positive := 3
  );
  port (
    clk_i   : in  std_ulogic;
    gprf_i  : in  gprf_in_t;
    gprf_o  : out gprf_out_t
  );
end gprf;

architecture rtl of gprf is
begin
  a : entity work.dsram 
    generic map (
      width_g => dmem_width_g,
      size_g  => gprf_size_g
    )
    port map (
      dat_o   => gprf_o.dat_a,
      adr_i   => gprf_i.adr_a,
      ena_i   => gprf_i.ena,
      dat_w_i => gprf_i.dat_w,
      adr_w_i => gprf_i.adr_w,
      wre_i   => gprf_i.wre,
      clk_i   => clk_i
    );

  b : entity work.dsram 
    generic map (
      width_g => dmem_width_g,
      size_g  => gprf_size_g
    )
    port map (
      dat_o   => gprf_o.dat_b,
      adr_i   => gprf_i.adr_b,
      ena_i   => gprf_i.ena,
      dat_w_i => gprf_i.dat_w,
      adr_w_i => gprf_i.adr_w,
      wre_i   => gprf_i.wre,
      clk_i   => clk_i
    );
  
end architecture rtl;


