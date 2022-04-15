library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.core_pkg.all;
use work.func_pkg.all;
use work.inst_pkg.all;
use work.decoupled_io_pkg.all;

entity exec is
  generic (
    use_m_ext_g    : boolean;
    core_id_g      : integer
  );
  port (
    clk_i          : in  std_ulogic;
    reset_n_i      : in  std_ulogic;
    init_i         : in  std_ulogic;
    en_i           : in  std_ulogic;
    exec_i         : in  execute_in_t;
    exec_o         : out execute_out_t;
    exec_comb_o    : out execute_comb_out_t;
    status_o       : out csr_status_t;
    receive_req_i  : in  decoupled_io_req_t;
    receive_rsp_o  : out decoupled_io_rsp_t;
    transmit_req_o : out decoupled_io_req_t;
    transmit_rsp_i : in  decoupled_io_rsp_t;
    ready_o        : out std_ulogic
  );
end entity exec;

architecture rtl of exec is
  ------------------------------------------------------------------------------
  -- multiply (unsigned * unsigned, signed * signed, signed * unsigned)
  ------------------------------------------------------------------------------
  function multiply(a, b : std_ulogic_vector; mode : mul_op_t) return std_ulogic_vector is
    variable mul_opa        : unsigned(a'length-1 downto 0);
    variable mul_opb        : unsigned(b'length-1 downto 0);
    variable mul_result     : unsigned(mul_opa'length + mul_opb'length-1 downto 0);
    variable mul_neg_result : unsigned(mul_opa'length + mul_opb'length-1 downto 0);
    variable result         : unsigned(mul_opa'length + mul_opb'length-1 downto 0);
  begin

    case(mode) is
      when MUL_MULU => mul_opa := unsigned(a);
                       mul_opb := unsigned(b);

      when MUL_MULS => if(a(a'left) = '1') then
                         mul_opa := unsigned(not a) + 1;
                       else
                         mul_opa := unsigned(a);
                       end if;

                       if(b(b'left) = '1') then
                         mul_opb := unsigned(not b) + 1;
                       else
                         mul_opb := unsigned(b);
                       end if;

      when others   => if(a(a'left) = '1') then
                         mul_opa := unsigned(not a) + 1;
                       else
                         mul_opa := unsigned(a);
                       end if;
                       mul_opb   := unsigned(b);
    end case;

    mul_result     := mul_opa * mul_opb;
    mul_neg_result := (not mul_result) + 1; 

    case(mode) is
      when MUL_MULU => result := mul_result;

      when MUL_MULS => if(a(a'left) /= b(b'left)) then
                         result := mul_neg_result;
                       else
                         result := mul_result;
                       end if;

      when others   => if(a(a'left) = '1') then
                         result := mul_neg_result;
                       else
                         result := mul_result;
                       end if;
    end case;
    return std_ulogic_vector(result);
  end function multiply;

  type reg_t is record
    exec       : execute_out_t;
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    exec       => dflt_execute_out_c
  );

  signal dividend             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  signal divider              : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  signal div_blocki0_quotient : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  signal div_blocki0_reminder : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  signal div_blocki0_ready    : std_ulogic;

  signal csrf                 : csrf_in_t;
  signal csrfi0_csrf          : csrf_out_t;

  signal ena                  : std_ulogic;
  signal r, rin               : reg_t;

begin
  ------------------------------------------------------------------------------
  -- divition unit
  ------------------------------------------------------------------------------
  div_blocki0: block is
    signal start          : std_ulogic;
    signal sign           : std_ulogic;
    signal divi0_quotient : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    signal divi0_reminder : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    signal divi0_ready    : std_ulogic;
  begin
    div_geni0: if use_m_ext_g generate

      start <= '1' when exec_i.ctrl_ex.alu_op = ALU_DIV or exec_i.ctrl_ex.alu_op = ALU_REM else '0';
      sign  <= '1' when exec_i.ctrl_ex.div_op = DIV_DIVS else '0';

      divi0: entity work.serial_divider
        generic map (
          width_g => CFG_DMEM_WIDTH
        )
        port map (
          clk_i      => clk_i,
          reset_n_i  => reset_n_i,
          en_i       => en_i,
          init_i     => init_i,
          start_i    => start,
          signed_i   => sign,
          dividend_i => dividend,
          divider_i  => divider,
          quotient_o => divi0_quotient,
          reminder_o => divi0_reminder,
          ready_o    => divi0_ready
        );
    end generate div_geni0;

    div_blocki0_ready    <= divi0_ready    when use_m_ext_g = true else '1';
    div_blocki0_quotient <= divi0_quotient when use_m_ext_g = true else (others=>'0');
    div_blocki0_reminder <= divi0_reminder when use_m_ext_g = true else (others=>'0');

  end block div_blocki0;

  ------------------------------------------------------------------------------
  -- csrf TODO: add csr feature to be generic
  ------------------------------------------------------------------------------
  csrfi0: entity work.csrf
    generic map (
      core_id_g => core_id_g
    )
    port map (
      clk_i          => clk_i,
      reset_n_i      => reset_n_i,
      init_i         => init_i,
      en_i           => ena,
      csrf_i         => csrf,
      csrf_o         => csrfi0_csrf,
      receive_req_i  => receive_req_i,
      receive_rsp_o  => receive_rsp_o,
      transmit_req_o => transmit_req_o,
      transmit_rsp_i => transmit_rsp_i
    );

  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, exec_i, div_blocki0_quotient, div_blocki0_reminder, div_blocki0_ready, csrfi0_csrf) is
    variable v             : reg_t;
    variable alu_src_a     : std_ulogic_vector(CFG_DMEM_WIDTH-1   downto 0);
    variable alu_src_b     : std_ulogic_vector(CFG_DMEM_WIDTH-1   downto 0);
    variable result        : std_ulogic_vector(CFG_DMEM_WIDTH-1   downto 0);
    variable result_add    : std_ulogic_vector(CFG_DMEM_WIDTH-1   downto 0);
    variable result_mul    : std_ulogic_vector(2*CFG_DMEM_WIDTH-1 downto 0);
    variable zero          : std_ulogic;
    variable sign_bits     : std_ulogic_vector(2 downto 0);
    variable carry         : std_ulogic;
    variable carry_out     : std_ulogic;
    variable overflow      : std_ulogic;
    variable negative      : std_ulogic;
    variable dat_a         : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable dat_b         : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable sel_dat_a     : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable sel_dat_b     : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0); 
    variable mem_result    : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable branch        : std_ulogic;
    variable branch_target : std_ulogic_vector(CFG_IMEM_SIZE-1 downto 0);
  begin
    v := r;

    ----------------------------------------------------------------------------
    -- write-back forward and r0 = zero logic
    ----------------------------------------------------------------------------
    sel_dat_a     := select_register_data(exec_i.dat_a, exec_i.reg_a, exec_i.fwd_dec_result, fwd_cond(exec_i.fwd_dec.reg_write, exec_i.fwd_dec.reg_d, exec_i.reg_a));
    sel_dat_b     := select_register_data(exec_i.dat_b, exec_i.reg_b, exec_i.fwd_dec_result, fwd_cond(exec_i.fwd_dec.reg_write, exec_i.fwd_dec.reg_d, exec_i.reg_b));

    ----------------------------------------------------------------------------
    -- conditionally flush execution stage in case of control hazards
    ----------------------------------------------------------------------------
    if(r.exec.flush_ex = '1') then
      v.exec.ctrl_mem.mem_write := '0';
      v.exec.ctrl_mem.mem_read  := '0';
      v.exec.ctrl_wrb.reg_write := '0';
      v.exec.ctrl_wrb.reg_d     := (others=>'0');
    else
      v.exec.ctrl_mem           := exec_i.ctrl_mem;
      v.exec.ctrl_wrb           := exec_i.ctrl_wrb;
    end if;

    ----------------------------------------------------------------------------
    -- if we got forwarded a memory result we migth need to align the data first
    ----------------------------------------------------------------------------
    if(exec_i.ctrl_mem_wrb.mem_read = '1') then
      mem_result := align_mem_load(exec_i.mem_result, exec_i.ctrl_mem_wrb.transfer_size, exec_i.alu_result(1 downto 0), exec_i.ctrl_mem_wrb.zero_extend);
    else
      mem_result := exec_i.alu_result;
    end if;

    ----------------------------------------------------------------------------
    -- multiplex between the various forward logic paths
    ----------------------------------------------------------------------------
    if(fwd_cond(r.exec.ctrl_wrb.reg_write, r.exec.ctrl_wrb.reg_d, exec_i.reg_a) = '1') then
      dat_a                     := r.exec.alu_result;
    elsif(fwd_cond(exec_i.fwd_mem.reg_write, exec_i.fwd_mem.reg_d, exec_i.reg_a) = '1') then
      dat_a                     := mem_result;
    else    
      dat_a                     := sel_dat_a;
    end if;

    if(fwd_cond(r.exec.ctrl_wrb.reg_write, r.exec.ctrl_wrb.reg_d, exec_i.reg_b) = '1') then
      dat_b                     := r.exec.alu_result;
    elsif(fwd_cond(exec_i.fwd_mem.reg_write, exec_i.fwd_mem.reg_d, exec_i.reg_b) = '1') then
      dat_b                     := mem_result;
    else
      dat_b                     := sel_dat_b;
    end if;

    ----------------------------------------------------------------------------
    -- forward to memory stage also
    ----------------------------------------------------------------------------
    v.exec.dat_b                := dat_b;

    ----------------------------------------------------------------------------
    -- set the first operand of the ALU
    ----------------------------------------------------------------------------
    case(exec_i.ctrl_ex.alu_src_a) is
      when ALU_SRC_PC       => alu_src_a := sign_extend(exec_i.pc, '0', CFG_DMEM_WIDTH);
      when ALU_SRC_ZERO     => alu_src_a := (others => '0');
      when ALU_SRC_ZIMM     => alu_src_a := sign_extend(exec_i.reg_a, '0', CFG_DMEM_WIDTH);
      when others           => alu_src_a := dat_a;
    end case;

    ----------------------------------------------------------------------------
    -- set the second operand of the ALU
    ----------------------------------------------------------------------------
    case(exec_i.ctrl_ex.alu_src_b) is
      when ALU_SRC_IMM      => alu_src_b := exec_i.imm;
      when ALU_SRC_NOT_IMM  => alu_src_b := not exec_i.imm;
      when ALU_SRC_NOT_REGB => alu_src_b := not dat_b;
      when others           => alu_src_b := dat_b;
    end case;

    ----------------------------------------------------------------------------
    -- determine value of carry in
    ----------------------------------------------------------------------------
    case(exec_i.ctrl_ex.carry) is
      when CARRY_ONE   => carry := '1';
      when CARRY_ARITH => carry := alu_src_a(CFG_DMEM_WIDTH-1);
      when others      => carry := '0';
    end case;

    result_add := add(alu_src_a, alu_src_b, carry);

    ----------------------------------------------------------------------------
    -- generate flags used to evaluate branch condition
    ----------------------------------------------------------------------------
    zero      := is_zero(result_add(result_add'left-1 downto 0));
    negative  := result_add(CFG_DMEM_WIDTH-1);
    sign_bits := result_add(CFG_DMEM_WIDTH-1) & alu_src_a(CFG_DMEM_WIDTH-1) & alu_src_b(CFG_DMEM_WIDTH-1);

    case(sign_bits) is
      when "001" | "010" | "011" | "111" 
                  => carry_out := '1';
      when others => carry_out := '0';
    end case;

    overflow  := (    result_add(CFG_DMEM_WIDTH-1) and not alu_src_a(CFG_DMEM_WIDTH-1) and not alu_src_b(CFG_DMEM_WIDTH-1)) or
                 (not result_add(CFG_DMEM_WIDTH-1) and     alu_src_a(CFG_DMEM_WIDTH-1) and     alu_src_b(CFG_DMEM_WIDTH-1));

    ----------------------------------------------------------------------------
    -- multiplication
    ----------------------------------------------------------------------------
    if(use_m_ext_g) then
      result_mul := multiply(dat_a, dat_b, exec_i.ctrl_ex.mul_op);
    else
      result_mul := (others=>'0');
    end if;

    ----------------------------------------------------------------------------
    -- arithmetic ALU operations
    ----------------------------------------------------------------------------
    case(exec_i.ctrl_ex.alu_op) is
      when ALU_ADD         => result := result_add;
      when ALU_OR          => result := (alu_src_a or alu_src_b);
      when ALU_AND         => result := (alu_src_a and alu_src_b);
      when ALU_XOR         => result := (alu_src_a xor alu_src_b);
      when ALU_SHIFT_LEFT  => result := shift_left(alu_src_a, alu_src_b(4 downto 0));
      when ALU_SHIFT_RIGHT => result := shift_right(alu_src_a, alu_src_b(4 downto 0), carry);
      when ALU_UCOMP       => result := (result'left downto 1 => '0') & not carry_out;
      when ALU_COMP        => result := (result'left downto 1 => '0') & (negative xor overflow);
      when ALU_MUL         => result := result_mul(CFG_DMEM_WIDTH-1 downto 0);
      when ALU_MULH        => result := result_mul(2*CFG_DMEM_WIDTH-1 downto CFG_DMEM_WIDTH);
      when ALU_DIV         => result := div_blocki0_quotient;
      when ALU_REM         => result := div_blocki0_reminder;
      when others          => null;
    end case;
    
    ----------------------------------------------------------------------------
    --mux with csr data
    ----------------------------------------------------------------------------
    if(exec_i.ctrl_ex.csr.csr_en = '1') then
      result := csrfi0_csrf.rdata;
    end if;

    ----------------------------------------------------------------------------
    -- mux result with PC increment 
    ----------------------------------------------------------------------------
    case(exec_i.ctrl_ex.branch_cond) is
      when JAL | JALR => v.exec.alu_result := sign_extend(std_ulogic_vector(unsigned(exec_i.pc(exec_i.pc'left downto 2)) + 1) & "00", '0', CFG_DMEM_WIDTH);
      when others     => v.exec.alu_result := result;
    end case;

    ----------------------------------------------------------------------------
    -- branch target address generation
    ----------------------------------------------------------------------------
    case(exec_i.ctrl_ex.branch_cond) is
      when JAL | JALR => branch_target := result(result'left downto 1) & '0';
      when others     => branch_target := std_ulogic_vector(signed(exec_i.pc(exec_i.pc'left downto 1)) + signed(exec_i.imm(exec_i.pc'left downto 1))) & '0';
    end case;


    if(r.exec.flush_ex = '0') then
      if(csrfi0_csrf.exception = '1') then
        v.exec.ctrl_wrb.reg_write := '0';
        v.exec.ctrl_wrb.reg_d     := (others=>'0');
      end if;
    end if;

    if(csrfi0_csrf.exception = '1') then
      branch_target      := csrfi0_csrf.csr.evec;
    elsif(exec_i.ctrl_ex.csr.sret = '1') then
      branch_target      := csrfi0_csrf.csr.epc;
    end if;

    ----------------------------------------------------------------------------
    -- evaluate branch condition
    ----------------------------------------------------------------------------
    if(r.exec.flush_ex = '1') then
      branch := '0';
    elsif(csrfi0_csrf.exception = '1') then
      branch := '1';
    elsif(exec_i.ctrl_ex.csr.sret = '1') then
      branch := '1';
    else
      -- determine branch condition
      case(exec_i.ctrl_ex.branch_cond) is
        when JAL | JALR => branch := '1';
        when BEQ        => branch := zero;
        when BNE        => branch := not zero;
        when BLT        => branch := negative xor overflow;
        when BGE        => branch := not (negative xor overflow);
        when BLTU       => branch := not carry_out;
        when BGEU       => branch := carry_out;
        when others     => branch := '0';
      end case;
    end if;

    ----------------------------------------------------------------------------
    -- determine flush signals
    ----------------------------------------------------------------------------
    v.exec.flush_id := branch;
    v.exec.flush_ex := branch;

    ----------------------------------------------------------------------------
    -- drive divider
    ----------------------------------------------------------------------------
    dividend       <= dat_a;
    divider        <= dat_b;

    ----------------------------------------------------------------------------
    -- drive csrf
    ----------------------------------------------------------------------------
    csrf.flush     <= r.exec.flush_ex;
    csrf.pc        <= exec_i.pc;
    csrf.irq       <= exec_i.ctrl_ex.irq;
    csrf.hazard    <= exec_i.hazard;
    csrf.addr      <= exec_i.imm(csr_addr_width_c-1 downto 0);
    csrf.gprf_data <= alu_src_a;
    csrf.ctrl      <= exec_i.ctrl_ex.csr;



    ----------------------------------------------------------------------------
    -- drive module output
    ----------------------------------------------------------------------------
    exec_o         <= r.exec;
    exec_comb_o    <= (branch => branch, branch_target => branch_target);
    ready_o        <= div_blocki0_ready;    
    status_o       <= csrfi0_csrf.csr.status;
 
    rin            <= v;
  end process comb0;


  ------------------------------------------------------------------------------
  -- sync0
  ------------------------------------------------------------------------------
  ena <= en_i and div_blocki0_ready and not exec_i.stall;

  sync0: process(clk_i, reset_n_i) is
  begin
    if(reset_n_i = '0') then
      r <= dflt_reg_c;
    elsif(rising_edge(clk_i)) then
      if(ena = '1') then
        if(init_i = '1') then
          r <= dflt_reg_c;
        else
          r <= rin;
        end if;
      end if;
    end if;
  end process sync0;

end architecture rtl;

