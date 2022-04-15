library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.core_pkg.all;

entity fetch is
  port (    
    clk_i     : in  std_ulogic;
    reset_n_i : in  std_ulogic;
    init_i    : in  std_ulogic;
    en_i      : in  std_ulogic;
    fetch_i   : in  fetch_in_t;
    fetch_o   : out fetch_out_t;
    imem_o    : out imem_out_t
  );
end entity fetch;

architecture rtl of fetch is
  type reg_t is record
    pc      : unsigned(CFG_IMEM_SIZE-1 downto 0);
    next_pc : unsigned(CFG_IMEM_SIZE-1 downto 0);
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    pc      => (others=>'0'),
    next_pc => (others=>'0')
  );

  signal r, rin : reg_t;

begin
  imem_o.ena <= en_i;
  
  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, fetch_i) is
    variable v   : reg_t;
    variable vpc : unsigned(CFG_IMEM_SIZE-1 downto 0);
  begin
    v         := r;

    v.next_pc := r.pc;

    if(fetch_i.hazard = '1' or fetch_i.stall = '1') then
      v.pc    := r.pc;
    elsif(fetch_i.branch = '1') then
      v.pc    := unsigned(fetch_i.branch_target);
    else
      v.pc    := (r.pc(r.pc'left downto 2) + 1) & "00";
    end if;

    fetch_o.pc <= std_ulogic_vector(r.next_pc);
    imem_o.adr <= std_ulogic_vector(r.pc);
   
    rin <= v;
  end process comb0;
  
  ------------------------------------------------------------------------------
  -- sync0
  ------------------------------------------------------------------------------
  sync0: process(clk_i, reset_n_i) is
  begin
    if(reset_n_i = '0') then
      r <= dflt_reg_c;
    elsif(rising_edge(clk_i)) then
      if(en_i = '1') then
        if(init_i = '1') then
          r <= dflt_reg_c;
        else
          r <= rin;
        end if;
      end if;
    end if;
  end process sync0;

end architecture rtl;

