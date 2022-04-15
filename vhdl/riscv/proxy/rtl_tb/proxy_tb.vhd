library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.decoupled_io_pkg.all;

entity proxy_tb is
end entity proxy_tb;

architecture beh of proxy_tb is

  signal sim_done      : boolean   := false;
  signal clk           : std_logic := '0';
  signal reset_n       : std_logic := '0';
  constant clk_cycle_c : time      := 10 ns;

  -- memory interface (host --> memory)
  signal duti0_host2mem_req    : decoupled_mem_req_t;
  signal host2mem_rsp          : decoupled_mem_rsp_t  := dflt_decoupled_mem_rsp_c;

  -- memory interface (memory --> host)
  signal mem2host_req          : decoupled_mem_req_t  := dflt_decoupled_mem_req_c;
  signal duti0_mem2host_rsp    : decoupled_mem_rsp_t;

  -- target interface (host --> target)
  signal duti0_host2target_req : decoupled_io_req_t;
  signal host2target_rsp       : decoupled_io_rsp_t  := dflt_decoupled_io_rsp_c;

  -- target interface (target --> host)
  signal target2host_req       : decoupled_io_req_t  := dflt_decoupled_io_req_c;
  signal duti0_target2host_rsp : decoupled_io_rsp_t;

begin
  ------------------------------------------------------------------------------
  -- clock and reset
  ------------------------------------------------------------------------------
  clk     <= not clk after clk_cycle_c/2 when sim_done = false;
  reset_n <= '1' after 500 ns;

  ------------------------------------------------------------------------------
  -- proxy
  ------------------------------------------------------------------------------
  dut: entity work.hostif_proxy
    port map (
      clk_i             => clk,
      reset_n_i         => reset_n,
      sim_done_i        => sim_done,
      host2mem_req_o    => duti0_host2mem_req,
      host2mem_rsp_i    => host2mem_rsp,
      mem2host_req_i    => mem2host_req,
      mem2host_rsp_o    => duti0_mem2host_rsp,
      host2target_req_o => duti0_host2target_req,
      host2target_rsp_i => host2target_rsp,
      target2host_req_i => target2host_req,
      target2host_rsp_o => duti0_target2host_rsp
    );

  ------------------------------------------------------------------------------
  -- stimuli
  ------------------------------------------------------------------------------
  stim0: process is
  begin
    wait until reset_n = '1';
    wait until rising_edge(clk);

    -- fist 200 us reply with data all zero
    delay: while(now < 200 us) loop
      wait until clk'event and clk = '1' and duti0_host2target_req.valid = '1';
      target2host_req.valid     <= '1';
      target2host_req.bits.data <= (others=>'0');

      wait until clk'event and clk = '1' and duti0_target2host_rsp.ready = '1';
      target2host_req.valid     <= '0';
      target2host_req.bits.data <= (others=>'0');
    end loop delay;

    -- after 200 us reply with data equal to 1 to signal success to fesvr
    wait until clk'event and clk = '1' and duti0_host2target_req.valid = '1';
    target2host_req.valid     <= '1';
    target2host_req.bits.data <= std_ulogic_vector(to_unsigned(1, target2host_req.bits.data'length));

    wait until clk'event and clk = '1' and duti0_target2host_rsp.ready = '1';
    target2host_req.valid     <= '0';
    target2host_req.bits.data <= (others=>'0');

    --sim_done <= true;
    --wait for 0 ns; -- allow proxy to close pipes after being triggered by sim_done
    --assert false report "Simulation finished successfully!" severity failure;
    wait;
  end process stim0;


end architecture beh;

