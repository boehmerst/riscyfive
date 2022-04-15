library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.branch_predict_pkg.all;

entity branch_predict is
  generic (
    ways_g     : positive := 1
  );
  port (
    clk_i      : in  std_ulogic;
    reset_n_i  : in  std_ulogic;
    en_i       : in  std_ulogic;
    init_i     : in  std_ulogic;
    request_i  : in  predict_request_t;
    update_i   : in  predict_update_t;
    response_o : out predict_response_t
  );
end entity branch_predict;

architecture rtl of branch_predict is

  type set_table_t is array(natural range 0 to ways_g-1) of branch_buffer_t;
  constant dflt_set_table_c : set_table_t := (others=>dflt_branch_buffer_c);

  type reg_t is record
    set : set_table_t;
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    set => dflt_set_table_c
  );
  
  signal r, rin : reg_t;
  
begin
  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, request_i, update_i) is
    variable v                           : reg_t;
    variable req_tag                     : std_ulogic_vector(target_width_c-log2_btb_size_c-1 downto 0);
    variable req_index                   : natural range 0 to 2**log2_btb_size_c-1;
    variable req_history                 : natural range 0 to 2**history_size_c-1;
    variable req_entry                   : pattern_history_entry_t;
    variable response                    : predict_response_t;
    variable update_tag                  : std_ulogic_vector(target_width_c-log2_btb_size_c-1 downto 0);
    variable update_index                : natural range 0 to 2**log2_btb_size_c-1;
    variable update_history_index        : natural range 0 to 2**history_size_c-1;
    variable update_local_history        : std_ulogic_vector(history_size_c-1 downto 0);
    variable update_entry                : pattern_history_entry_t;
    variable update_entry_avail          : std_ulogic;
    variable update_entry_set            : natural range 0 to ways_g-1;
    variable update_free_avail           : std_ulogic;
    variable update_free_set             : natural range 0 to ways_g-1;
    variable update_insert_history_index : natural range 0 to 2**history_size_c-1;
    variable update_insert_set           : natural range 0 to ways_g-1;
  begin
    v := r;
    
    ----------------------------------------------------------------------------
    -- find which set (if any) to hold current PC
    ----------------------------------------------------------------------------
    req_tag     := request_i.pc(request_i.pc'left downto log2_btb_size_c);
    req_index   := to_integer(unsigned(request_i.pc(log2_btb_size_c-1 downto 0)));
    req_entry   := dflt_pattern_history_entry_c;
    
    find_set0: for i in 0 to ways_g-1 loop
      if(r.set(i)(req_index).tag = req_tag) then
        req_history   := to_integer(unsigned(r.set(i)(req_index).local_history));
        req_entry     := r.set(i)(req_index).pattern_history(req_history);
        exit find_set0;
      end if;
    end loop find_set0;
    
    response.valid    := req_entry.valid;
    response.predict  := req_entry.predict(1);
    response.target   := req_entry.target;
    
    if(response.valid = '0') then
      response.target := request_i.next_pc;
    end if;
    
    ----------------------------------------------------------------------------
    -- update the buffer entry (may be removed from set already!)
    ----------------------------------------------------------------------------
    update_tag           := update_i.pc(update_i.pc'left downto log2_btb_size_c);
    update_index         := to_integer(unsigned(update_i.pc(log2_btb_size_c-1 downto 0)));
    update_local_history := (others=>'0');
    update_history_index := 0;
    update_entry         := dflt_pattern_history_entry_c;
    update_entry_avail   := '0';
    update_entry_set     := 0;
    update_free_avail    := '0';
    update_free_set      := 0;
    
    -- find the set to be updated... if still available
    find_set1: for i in 0 to ways_g-1 loop
      if(r.set(i)(update_index).tag = update_tag) then
        update_local_history := r.set(i)(update_index).local_history;
        update_history_index := to_integer(unsigned(update_local_history));
        update_entry         := r.set(i)(req_index).pattern_history(update_history_index);
        update_entry_avail   := '1';
        update_entry_set     := i;
        exit find_set1;
      end if;
    end loop find_set1;    
    
    -- find a free set... if any free is available
    find_free_set0: for i in 0 to ways_g-1 loop
      if(r.set(i)(update_index).tag = free_tag_c) then
        update_free_avail    := '1';
        update_free_set      := i;
        exit find_free_set0;
      end if;
    end loop find_free_set0;    
    
    if(update_i.wr = '1') then
      update_local_history     := update_local_history(update_local_history'left-1 downto 0) & update_i.taken;
    
      if(update_i.taken = '1') then
        if(update_entry.predict /= predict_strongly_taken_c) then
          update_entry.predict := update_entry.predict + 1;
        end if;
      else
        if(update_entry.predict /= predict_strongly_not_taken_c) then
          update_entry.predict := update_entry.predict - 1;
        end if;
      end if;
    
      update_entry.valid            := '1';
      update_entry.target           := update_i.target;
      
      if(update_entry_avail = '1') then
        update_insert_set           := update_entry_set;
        update_insert_history_index := update_history_index; 
      elsif(update_free_avail = '1') then
        update_insert_set           := update_free_set;
        update_insert_history_index := to_integer(unsigned(update_local_history));
      else
        -- TODO: implement proper replacement strategy
        update_insert_set           := 0;
        update_insert_history_index := to_integer(unsigned(update_local_history));
      end if;
      
      v.set(update_insert_set)(update_index).tag                                          := update_tag;
      v.set(update_insert_set)(update_index).local_history                                := update_local_history;
      v.set(update_insert_set)(update_index).pattern_history(update_insert_history_index) := update_entry;
    end if;
    
    ----------------------------------------------------------------------------
    -- drive module output
    ----------------------------------------------------------------------------    
    response_o <= response;
    
    rin        <= v;
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

