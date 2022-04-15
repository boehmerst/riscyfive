library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;

package branch_predict_pkg is

  constant target_width_c  : integer := CFG_IMEM_SIZE;
  constant log2_btb_size_c : integer := 4;
  constant history_size_c  : integer := 2;
  
  type predict_request_t is record
    pc      : std_ulogic_vector(target_width_c-1 downto 0);
    next_pc : std_ulogic_vector(target_width_c-1 downto 0);
  end record predict_request_t;
  constant dflt_predict_request_c : predict_request_t :=(
    pc      => (others=>'0'),
    next_pc => (others=>'0')
  );
  
  type predict_update_t is record
    pc     : std_ulogic_vector(target_width_c-1 downto 0);
    target : std_ulogic_vector(target_width_c-1 downto 0);
    taken  : std_ulogic;
    wr     : std_ulogic;
  end record predict_update_t;
  constant dflt_predict_update_c : predict_update_t :=(
    pc     => (others=>'0'),
    target => (others=>'0'),
    taken  => '0',
    wr     => '0'
  );
  
  type predict_response_t is record
    valid   : std_ulogic;
    predict : std_ulogic;
    target  : std_ulogic_vector(target_width_c-1 downto 0);
  end record predict_response_t;
  
  constant predict_strongly_not_taken_c : unsigned(1 downto 0) := "00";
  constant predict_weakly_not_taken_c   : unsigned(1 downto 0) := "01";
  constant predict_weakly_taken_c       : unsigned(1 downto 0) := "10";
  constant predict_strongly_taken_c     : unsigned(1 downto 0) := "11";

  type pattern_history_entry_t is record
    predict : unsigned(1 downto 0);
    valid   : std_ulogic;
    target  : std_ulogic_vector(target_width_c-1 downto 0);
  end record pattern_history_entry_t;
  constant dflt_pattern_history_entry_c : pattern_history_entry_t :=(
    predict => predict_weakly_taken_c,
    valid   => '0',
    target  => (others=>'0')
  );
  
  type pattern_history_table_t is array(natural range 0 to 2**history_size_c-1) of pattern_history_entry_t;
  constant dflt_pattern_history_table_c : pattern_history_table_t := (others=>dflt_pattern_history_entry_c);
  
  constant free_tag_c : std_ulogic_vector(target_width_c-log2_btb_size_c-1 downto 0) := (others=>'0');
  
  -- the tag tells which PC is stored in the actual set
  -- the history tells us about the local branch history assosiated with the PC
  type branch_buffer_entry_t is record
    tag             : std_ulogic_vector(target_width_c-log2_btb_size_c-1 downto 0);
    local_history   : std_ulogic_vector(history_size_c-1 downto 0);
    pattern_history : pattern_history_table_t;
  end record branch_buffer_entry_t;
  constant dflt_branch_buffer_entry_c : branch_buffer_entry_t :=(
    tag             => free_tag_c,
    local_history   => (others=>'0'),
    pattern_history => dflt_pattern_history_table_c
  );
  
  type branch_buffer_t is array(natural range 0 to 2**log2_btb_size_c-1) of branch_buffer_entry_t;
  constant dflt_branch_buffer_c : branch_buffer_t := (others=>dflt_branch_buffer_entry_c);

end package branch_predict_pkg;

