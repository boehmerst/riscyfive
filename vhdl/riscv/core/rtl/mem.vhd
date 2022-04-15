library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.core_pkg.all;
use work.func_pkg.all;

entity mem is
  port (
    clk_i      : in  std_ulogic;
    reset_n_i  : in  std_ulogic;
    init_i     : in  std_ulogic;
    en_i       : in  std_ulogic;
    mem_o      : out mem_out_t;
    mem_i      : in  mem_in_t;
    dmem_o     : out dmem_out_t
  );
end entity mem;

architecture rtl of mem is

  type reg_t is record
    mem          : mem_out_t;
    mem_addr_sel : std_ulogic_vector(log2ceil(CFG_DMEM_WIDTH/8)-1 downto 0);
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    mem          => dflt_mem_out_c,
    mem_addr_sel => (others=>'0')
  );

  signal r, rin : reg_t;

begin
  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, mem_i) is
    variable v            : reg_t;
    variable intermediate : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable mem_result   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  begin
    v := r;

    ----------------------------------------------------------------------------
    -- simple delay latch to provide to WB stage and forward to EX
    ----------------------------------------------------------------------------
    v.mem.ctrl_wrb                    := mem_i.ctrl_wrb;
    v.mem.ctrl_mem_wrb.mem_read       := mem_i.ctrl_mem.mem_read;
    v.mem.ctrl_mem_wrb.transfer_size  := mem_i.ctrl_mem.transfer_size;
    v.mem.ctrl_mem_wrb.zero_extend    := mem_i.ctrl_mem.zero_extend;
    v.mem.alu_result                  := mem_i.alu_result;

    ----------------------------------------------------------------------------
    -- latch to be used for reordering mem_result (see below)
    ----------------------------------------------------------------------------
    v.mem_addr_sel                    := mem_i.mem_addr(v.mem_addr_sel'range);

    ----------------------------------------------------------------------------
    -- forward memory result
    ----------------------------------------------------------------------------
    if(CFG_MEM_FWD_WRB = true and (r.mem.ctrl_mem_wrb.mem_read and compare(mem_i.ctrl_wrb.reg_d, r.mem.ctrl_wrb.reg_d)) = '1') then
      intermediate                    := align_mem_load(mem_i.mem_result, r.mem.ctrl_mem_wrb.transfer_size, r.mem_addr_sel, r.mem.ctrl_mem_wrb.zero_extend);
      mem_result                      := align_mem_store(intermediate, mem_i.ctrl_mem.transfer_size);
    else
      -- TODO: could be moved to execution stage to shorten the combinatorial depth
      mem_result                      := align_mem_store(mem_i.dat_b, mem_i.ctrl_mem.transfer_size);
    end if;

    ---------------------------------------------------------------------------
    -- drive module outputs
    ---------------------------------------------------------------------------
    mem_o       <= r.mem;
    
    dmem_o.dat  <= mem_result;
    dmem_o.sel  <= decode_mem_store(mem_i.mem_addr(1 downto 0), mem_i.ctrl_mem.transfer_size);
    dmem_o.we   <= mem_i.ctrl_mem.mem_write;
    dmem_o.adr  <= mem_i.mem_addr(CFG_DMEM_SIZE-1 downto 0);
    dmem_o.ena  <= mem_i.ctrl_mem.mem_read or mem_i.ctrl_mem.mem_write;

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

