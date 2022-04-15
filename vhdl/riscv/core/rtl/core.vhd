library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.func_pkg.all;
use work.core_pkg.all;
use work.inst_pkg.all;
use work.decoupled_io_pkg.all;

entity core is
  generic (
    use_m_ext_g    : boolean := CFG_USE_M_EXT;
    core_id_g      : integer := 1
  );
  port (
    clk_i          : in  std_ulogic;
    reset_n_i      : in  std_ulogic;
    init_i         : in  std_ulogic;
    irq_i          : in  std_ulogic;
    wait_n_i       : in  std_ulogic;
    imem_o         : out imem_out_t;
    imem_i         : in  imem_in_t;
    dmem_o         : out dmem_out_t;
    dmem_i         : in  dmem_in_t;
    receive_req_i  : in  decoupled_io_req_t;
    receive_rsp_o  : out decoupled_io_rsp_t;
    transmit_req_o : out decoupled_io_req_t;
    transmit_rsp_i : in  decoupled_io_rsp_t
  );
end entity core;

architecture rtl of core is

  signal fetch                : fetch_in_t;
  signal fetchi0_fetch        : fetch_out_t;

  signal decode               : decode_in_t;
  signal decodei0_decode      : decode_out_t;
  signal decodei0_decode_comb : decode_comb_out_t;
  signal decodei0_gprf        : gprf_out_t;
  
  signal exec                 : execute_in_t;
  signal execi0_exec          : execute_out_t;
  signal execi0_exec_comb     : execute_comb_out_t;
  signal execi0_ready         : std_ulogic;
  signal execi0_status        : csr_status_t;

  signal mem                  : mem_in_t;
  signal memi0_mem            : mem_out_t;
  signal memi0_dmem           : dmem_out_t;
  
  signal dmem                 : dmem_in_t;  
  signal ena                  : std_ulogic;
  signal exec_ena             : std_ulogic;

begin
  ------------------------------------------------------------------------------
  -- enable for core and execution units
  ------------------------------------------------------------------------------
  ena      <= wait_n_i and execi0_ready;
  exec_ena <= wait_n_i;

  ------------------------------------------------------------------------------
  -- fetch (IF)
  ------------------------------------------------------------------------------
  fetch.hazard        <= decodei0_decode_comb.hazard;
  fetch.stall         <= decodei0_decode_comb.stall;
  fetch.branch        <= execi0_exec_comb.branch;
  fetch.branch_target <= execi0_exec_comb.branch_target;
  
  fetchi0: entity work.fetch
    port map (
      clk_i           => clk_i,
      reset_n_i       => reset_n_i,
      init_i          => init_i,
      en_i            => ena,
      fetch_i         => fetch,
      fetch_o         => fetchi0_fetch,
      imem_o          => imem_o
    );

  ------------------------------------------------------------------------------
  -- decode (ID, OF and WB)
  ------------------------------------------------------------------------------
  decode.irq.pending  <= execi0_status.ip; -- TODO: implement external irq
  decode.irq.mask     <= execi0_status.im;
  decode.irq.enable   <= execi0_status.ei;
  decode.pc           <= fetchi0_fetch.pc;
  decode.inst         <= imem_i.dat;
  decode.ctrl_wrb     <= memi0_mem.ctrl_wrb;
  decode.ctrl_mem_wrb <= memi0_mem.ctrl_mem_wrb;
  decode.mem_result   <= dmem.dat;
  decode.alu_result   <= memi0_mem.alu_result;
  decode.flush_id     <= execi0_exec.flush_id;
  decode.mode         <= execi0_status.s;
    
  decodei0: entity work.decode
    generic map (
      use_m_ext_g     => use_m_ext_g,
      use_dbg_g       => true
    )
    port map (
      clk_i           => clk_i,
      reset_n_i       => reset_n_i,
      init_i          => init_i,
      en_i            => ena,
      decode_i        => decode,
      decode_o        => decodei0_decode,
      decode_comb_o   => decodei0_decode_comb,
      gprf_o          => decodei0_gprf
    );

  ------------------------------------------------------------------------------
  -- scalar execution unit (EX)
  ------------------------------------------------------------------------------
  exec.fwd_dec        <= decodei0_decode.fwd_dec;
  exec.fwd_dec_result <= decodei0_decode.fwd_dec_result;

  exec.dat_a          <= decodei0_gprf.dat_a;
  exec.dat_b          <= decodei0_gprf.dat_b;
  exec.reg_a          <= decodei0_decode.reg_a;
  exec.reg_b          <= decodei0_decode.reg_b;

  exec.imm            <= decodei0_decode.imm;
  exec.pc             <= decodei0_decode.pc;
  exec.stall          <= decodei0_decode.stall;
  exec.ctrl_wrb       <= decodei0_decode.ctrl_wrb;
  exec.ctrl_mem       <= decodei0_decode.ctrl_mem;
  exec.ctrl_ex        <= decodei0_decode.ctrl_ex;

  exec.fwd_mem        <= memi0_mem.ctrl_wrb;
  exec.mem_result     <= dmem.dat;
  exec.alu_result     <= memi0_mem.alu_result;
  exec.ctrl_mem_wrb   <= memi0_mem.ctrl_mem_wrb;
  
  execi0: entity work.exec
    generic map (
      use_m_ext_g     => use_m_ext_g,
      core_id_g       => core_id_g
    )
    port map (
      clk_i           => clk_i,
      reset_n_i       => reset_n_i,
      init_i          => init_i,
      en_i            => exec_ena,
      exec_i          => exec,
      exec_o          => execi0_exec,
      exec_comb_o     => execi0_exec_comb,
      status_o        => execi0_status,
      receive_req_i   => receive_req_i,
      receive_rsp_o   => receive_rsp_o,
      transmit_req_o  => transmit_req_o,
      transmit_rsp_i  => transmit_rsp_i,
      ready_o         => execi0_ready
    );

  ------------------------------------------------------------------------------
  -- memory unit (MEM)
  ------------------------------------------------------------------------------
  mem.alu_result  <= execi0_exec.alu_result;
  mem.mem_addr    <= execi0_exec.alu_result;
  mem.dat_b       <= execi0_exec.dat_b;
  mem.ctrl_wrb    <= execi0_exec.ctrl_wrb;
  mem.ctrl_mem    <= execi0_exec.ctrl_mem;
  mem.mem_result  <= dmem.dat;
  
  memi0: entity work.mem
    port map (
      clk_i      => clk_i,
      reset_n_i  => reset_n_i,
      init_i     => init_i,
      en_i       => ena,
      mem_o      => memi0_mem,
      mem_i      => mem,
      dmem_o     => memi0_dmem
    );

  dmem_o  <= memi0_dmem;
  dmem    <= dmem_i;

end architecture rtl;

