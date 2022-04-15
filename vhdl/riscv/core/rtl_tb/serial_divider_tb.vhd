library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;

entity serial_divider_tb is
end entity serial_divider_tb;

architecture beh of serial_divider_tb is
  signal sim_done       : boolean   := false;
  signal clk            : std_logic := '0';
  signal reset_n        : std_logic := '0';
  constant clk_cycle_c  : time      := 10 ns;

  signal diva           : std_ulogic_vector(31 downto 0)  := (others=>'0');
  signal divb           : std_ulogic_vector(31 downto 0)  := (others=>'0');
  signal sign           : std_ulogic                      := '0';
  signal start          : std_ulogic                      := '0';
  signal divi0_quotient : std_ulogic_vector(31 downto 0);
  signal divi0_reminder : std_ulogic_vector(31 downto 0);
  signal divi0_ready    : std_ulogic;

begin
  ------------------------------------------------------------------------------
  -- clock and reset
  ------------------------------------------------------------------------------
  clk     <= not clk after clk_cycle_c/2 when sim_done = false;
  reset_n <= '1' after 500 ns;

  ------------------------------------------------------------------------------
  -- serial divider
  ------------------------------------------------------------------------------
  divi0: entity work.serial_divider
    generic map (
      width_g => 32
    )
    port map (
      clk_i      => clk,
      reset_n_i  => reset_n,
      en_i       => '1',
      init_i     => '0',
      start_i    => start,
      signed_i   => sign,
      dividend_i => diva,
      divider_i  => divb,
      quotient_o => divi0_quotient,
      reminder_o => divi0_reminder,
      ready_o    => divi0_ready
    );
 
  ------------------------------------------------------------------------------
  -- stimuli
  ------------------------------------------------------------------------------
  stim0: process is
  begin
    wait until reset_n = '1';
    wait until rising_edge(clk);

    wait for 100 ns;
    wait until rising_edge(clk);

    diva  <= std_ulogic_vector(to_unsigned(4, diva'length));
    divb  <= std_ulogic_vector(to_unsigned(8, diva'length));
    start <= '1';

    wait until rising_edge(clk);
    start <= '0';

    wait for 100 us;
    sim_done <= true;
    assert false report "Simulation finished successfully!" severity failure;
    wait;
  end process stim0;

end architecture beh;

