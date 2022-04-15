library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.func_pkg.all;
use work.inst_pkg.all;

package core_pkg is

  constant C_8_ZEROS  : std_ulogic_vector ( 7 downto 0) := (others => '0');
  constant C_16_ZEROS : std_ulogic_vector (15 downto 0) := (others => '0');
  constant C_24_ZEROS : std_ulogic_vector (23 downto 0) := (others => '0');
  constant C_32_ZEROS : std_ulogic_vector (31 downto 0) := (others => '0');

  type alu_op_t           is (ALU_ADD, ALU_OR, ALU_AND, ALU_XOR, ALU_SHIFT_LEFT, ALU_SHIFT_RIGHT, ALU_UCOMP, ALU_COMP, ALU_MUL, ALU_MULH, ALU_DIV, ALU_REM);
  type src_a_t            is (ALU_SRC_REGA, ALU_SRC_PC, ALU_SRC_ZERO, ALU_SRC_ZIMM);
  type src_b_t            is (ALU_SRC_REGB, ALU_SRC_NOT_REGB, ALU_SRC_IMM, ALU_SRC_NOT_IMM);

  type mul_op_t           is (MUL_MULU, MUL_MULS, MUL_MULSU);
  type div_op_t           is (DIV_DIVU, DIV_DIVS);

  type carry_t            is (CARRY_ZERO, CARRY_ONE, CARRY_ARITH);
  type branch_condition_t is (NOP, JAL, JALR, BEQ, BNE, BLT, BGE, BLTU, BGEU);
  type transfer_size_t    is (WORD, HALFWORD, BYTE);

  type csr_op_t           is (CSR_N, CSR_S, CSR_C, CSR_W);

  type irq_ctrl_t is record
    enable            : std_ulogic;
    mask              : std_ulogic_vector(7 downto 0);
    pending           : std_ulogic_vector(7 downto 0); 
  end record irq_ctrl_t; 

  type csr_ctrl_t is record
    csr_op            : csr_op_t;
    csr_wr            : std_ulogic;
    csr_en            : std_ulogic;
    csr_priv          : std_ulogic;
    op_priv           : std_ulogic;
    syscall           : std_ulogic;
    sret              : std_ulogic;
    illegal           : std_ulogic;
  end record csr_ctrl_t;
  constant dflt_csr_ctrl_c : csr_ctrl_t :=(
    csr_op            => CSR_N,
    csr_wr            => '0',
    csr_en            => '0',
    csr_priv          => '0',
    op_priv           => '0',
    syscall           => '0',
    sret              => '0',
    illegal           => '0'
  );

  type csrf_in_t is record
    flush             : std_ulogic;
    pc                : std_ulogic_vector(CFG_IMEM_SIZE-1  downto 0);
    hazard            : std_ulogic;
    irq               : std_ulogic;
    addr              : std_ulogic_vector(csr_addr_width_c-1 downto 0);
    gprf_data         : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    ctrl              : csr_ctrl_t;
  end record csrf_in_t;

  type csrf_out_t is record
    csr               : csr_reg_t;
    rdata             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    exception         : std_ulogic;
  end record csrf_out_t;

  type ctrl_execution_t is record
    irq               : std_ulogic;
    alu_op            : alu_op_t;
    mul_op            : mul_op_t;
    div_op            : div_op_t;
    alu_src_a         : src_a_t;
    alu_src_b         : src_b_t;
    carry             : carry_t;
    branch_cond       : branch_condition_t;
    csr               : csr_ctrl_t;
 end record ctrl_execution_t;
 constant dflt_ctrl_execution_c : ctrl_execution_t :=(
    irq               => '0',
    alu_op            => ALU_ADD,
    mul_op            => MUL_MULU,
    div_op            => DIV_DIVU,
    alu_src_a         => ALU_SRC_REGA,
    alu_src_b         => ALU_SRC_REGB,
    carry             => CARRY_ZERO,
    branch_cond       => NOP,
    csr               => dflt_csr_ctrl_c
  );

  type ctrl_memory_t is record
    mem_write         : std_ulogic;
    mem_read          : std_ulogic;
    transfer_size     : transfer_size_t;
    zero_extend       : std_ulogic;
  end record ctrl_memory_t;
  constant dflt_ctrl_memory_c : ctrl_memory_t :=(
    mem_write         => '0',
    mem_read          => '0',
    transfer_size     => WORD,
    zero_extend       => '0'
  );

  type ctrl_memory_writeback_t is record
    mem_read          : std_ulogic;
    transfer_size     : transfer_size_t;
    zero_extend       : std_ulogic;
  end record ctrl_memory_writeback_t;
  constant dflt_ctrl_memory_writeback_c : ctrl_memory_writeback_t :=(
    mem_read          => '0',
    transfer_size     => WORD,
    zero_extend       => '0'
  );

  type forward_t is record
    reg_d             : std_ulogic_vector(CFG_GPRF_SIZE-1 downto 0);
    reg_write         : std_ulogic;
  end record forward_t;
  constant dflt_forward_c : forward_t :=(
    reg_d             => (others=>'0'),
    reg_write         => '0'
  );

  type imem_in_t is record
    dat               : std_ulogic_vector(CFG_IMEM_WIDTH-1 downto 0);
  end record imem_in_t;

  type imem_out_t is record
    adr               : std_ulogic_vector(CFG_IMEM_SIZE-1 downto 0);
    ena               : std_ulogic;
  end record imem_out_t;

  type fetch_in_t is record
    hazard            : std_ulogic;
    stall             : std_ulogic;
    branch            : std_ulogic;
    branch_target     : std_ulogic_vector(CFG_IMEM_SIZE-1 downto 0);
  end record fetch_in_t;

  type fetch_out_t is record
    pc                : std_ulogic_vector(CFG_IMEM_SIZE-1 downto 0);
  end record fetch_out_t;

  type gprf_in_t is record
    adr_a             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    adr_b             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    dat_w             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    adr_w             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    wre               : std_ulogic;
    ena               : std_ulogic;
  end record gprf_in_t;

  type gprf_out_t is record
    dat_a             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    dat_b             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  end record gprf_out_t;

  type decode_in_t is record
    irq               : irq_ctrl_t;
    pc                : std_ulogic_vector(CFG_IMEM_SIZE-1  downto 0);
    inst              : std_ulogic_vector(CFG_IMEM_WIDTH-1 downto 0);
    ctrl_wrb          : forward_t;
    ctrl_mem_wrb      : ctrl_memory_writeback_t;
    mem_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    alu_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    flush_id          : std_ulogic;
    mode              : std_ulogic;
  end record decode_in_t;

  type decode_out_t is record
    reg_a             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    reg_b             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    imm               : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    pc                : std_ulogic_vector(CFG_IMEM_SIZE-1  downto 0);
    stall             : std_ulogic;
    ctrl_ex           : ctrl_execution_t;
    ctrl_mem          : ctrl_memory_t;
    ctrl_wrb          : forward_t;
    fwd_dec_result    : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    fwd_dec           : forward_t;
  end record decode_out_t;
  constant dflt_decode_out_c : decode_out_t :=(
    reg_a             => (others=>'0'),
    reg_b             => (others=>'0'),  
    imm               => (others=>'0'),
    pc                => (others=>'0'),
    stall             => '0',
    ctrl_ex           => dflt_ctrl_execution_c,
    ctrl_mem          => dflt_ctrl_memory_c,
    ctrl_wrb          => dflt_forward_c,
    fwd_dec_result    => (others => '0'),
    fwd_dec           => dflt_forward_c
  );

  type decode_comb_out_t is record
    hazard            : std_ulogic;
    stall             : std_ulogic;
  end record decode_comb_out_t;

  type execute_in_t is record
    reg_a             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    dat_a             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    reg_b             : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    dat_b             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);  
    imm               : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    pc                : std_ulogic_vector(CFG_IMEM_SIZE-1  downto 0);
    stall             : std_ulogic;
    hazard            : std_ulogic;
    fwd_dec           : forward_t;
    fwd_dec_result    : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    fwd_mem           : forward_t;
    ctrl_ex           : ctrl_execution_t;
    ctrl_mem          : ctrl_memory_t;
    ctrl_wrb          : forward_t;
    ctrl_mem_wrb      : ctrl_memory_writeback_t;
    mem_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    alu_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  end record execute_in_t;

  type execute_out_t is record
    alu_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    dat_b             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    flush_id          : std_ulogic;
    flush_ex          : std_ulogic;
    ctrl_mem          : ctrl_memory_t;
    ctrl_wrb          : forward_t;
  end record execute_out_t;
  constant dflt_execute_out_c : execute_out_t :=(
    alu_result        => (others=>'0'),
    dat_b             => (others=>'0'),
    flush_id          => '0',
    flush_ex          => '0',
    ctrl_mem          => dflt_ctrl_memory_c,
    ctrl_wrb          => dflt_forward_c
  );

  type execute_comb_out_t is record
    branch            : std_ulogic;
    branch_target     : std_ulogic_vector(CFG_IMEM_SIZE-1 downto 0);
  end record execute_comb_out_t;

  type mem_in_t is record
    dat_b             : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    alu_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    mem_addr          : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    mem_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    ctrl_mem          : ctrl_memory_t;
    ctrl_wrb          : forward_t;
  end record mem_in_t;

  type mem_out_t is record
    alu_result        : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    ctrl_wrb          : forward_t;
    ctrl_mem_wrb      : ctrl_memory_writeback_t;
  end record mem_out_t;
  constant dflt_mem_out_c : mem_out_t :=(
    alu_result        => (others=>'0'),
    ctrl_wrb          => dflt_forward_c,
    ctrl_mem_wrb      => dflt_ctrl_memory_writeback_c
  );

  type dmem_in_t is record
    dat               : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  end record dmem_in_t;

  type dmem_out_t is record
    dat               : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    adr               : std_ulogic_vector(CFG_DMEM_SIZE-1  downto 0);
    sel               : std_ulogic_vector(3 downto 0);
    we                : std_ulogic;
    ena               : std_ulogic;
  end record dmem_out_t;
  constant dflt_dmem_out_c : dmem_out_t :=(
    dat               => (others=>'0'),
    adr               => (others=>'0'),
    sel               => (others=>'0'),
    we                => '0',
    ena               => '0'
  );

  function select_register_data (reg_dat, reg, wb_dat : std_ulogic_vector; write : std_ulogic) return std_ulogic_vector;
  function fwd_cond (reg_write : std_ulogic; reg_a, reg_d : std_ulogic_vector) return std_ulogic;
  function align_mem_load (data : std_ulogic_vector; size : transfer_size_t; address : std_ulogic_vector; zero_extend : std_ulogic) return std_ulogic_vector;
  function align_mem_store (data : std_ulogic_vector; size : transfer_size_t) return std_ulogic_vector;
  function decode_mem_store (address : std_ulogic_vector(1 downto 0); size : transfer_size_t) return std_ulogic_vector;

end package core_pkg;

package body core_pkg is

  --------------------------------------------------------------------------------
  -- This function select the register value:
  --   A) zero
  --   B) bypass value read from register file
  --   C) value from register file
  --------------------------------------------------------------------------------
  function select_register_data (reg_dat, reg, wb_dat : std_ulogic_vector; write : std_ulogic) return std_ulogic_vector is
    variable val : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  begin
    if(CFG_REG_FORCE_ZERO = true and is_zero(reg) = '1') then
      val := (others => '0');
    elsif(CFG_REG_FWD_WRB = true and write = '1') then
      val := wb_dat;
    else
      val := reg_dat;
    end if;
      return val;
  end function select_register_data;
    
  --------------------------------------------------------------------------------
  -- This function checks if a forwarding condition is met. The condition is met 
  -- of register A and D match
  -- and the signal needs to be written back to the register file
  --------------------------------------------------------------------------------
  function fwd_cond (reg_write : std_ulogic; reg_a, reg_d : std_ulogic_vector ) return std_ulogic is
  begin
    return reg_write and compare(reg_a, reg_d);
  end function fwd_cond;

  --------------------------------------------------------------------------------
  -- This function aligns the memory load operation (little endian decoding)
  --------------------------------------------------------------------------------
  function align_mem_load (data : std_ulogic_vector; size : transfer_size_t; address : std_ulogic_vector; zero_extend : std_ulogic) return std_ulogic_vector is
    variable sign : std_ulogic;
  begin
    case size is
      when byte => 
        case address(1 downto 0) is
          when "00"   => sign := not zero_extend and data(7);
                         return sign_extend(data( 7 downto  0), sign, CFG_DMEM_WIDTH);

          when "01"   => sign := not zero_extend and data(15);
                         return sign_extend(data(15 downto  8), sign, CFG_DMEM_WIDTH);

          when "10"   => sign := not zero_extend and data(23);
                         return sign_extend(data(23 downto 16), sign, CFG_DMEM_WIDTH);

          when "11"   => sign := not zero_extend and data(31);
                         return sign_extend(data(31 downto 24), sign, CFG_DMEM_WIDTH);

          when others => return C_32_ZEROS;
        end case;
      when halfword => 
        case address(1 downto 0) is
          when "00"   => sign := not zero_extend and data(15);
                         return sign_extend(data(15 downto  0), sign, CFG_DMEM_WIDTH);

          when "10"   => sign := not zero_extend and data(31);
                         return sign_extend(data(31 downto 16), sign, CFG_DMEM_WIDTH);

          when others => return C_32_ZEROS;
        end case;
      when others =>
        return data;
    end case;
  end function align_mem_load;

  --------------------------------------------------------------------------------
  -- This function repeats the operand to all positions in a memory store operation
  --------------------------------------------------------------------------------
  function align_mem_store (data : std_ulogic_vector; size : transfer_size_t) return std_ulogic_vector is
  begin
    case size is
      when byte     => return data( 7 downto 0) & data( 7 downto 0) & data(7 downto 0) & data(7 downto 0);
      when halfword => return data(15 downto 0) & data(15 downto 0);
      when others   => return data;
    end case;
  end function align_mem_store;

  --------------------------------------------------------------------------------
  -- This function selects the correct bytes for memory writes (little endian encoding)
  --------------------------------------------------------------------------------
  function decode_mem_store (address : std_ulogic_vector(1 downto 0); size : transfer_size_t) return std_ulogic_vector is
  begin
    case size is
      when BYTE =>
        case address is
          when "00"   => return "0001";
          when "01"   => return "0010";
          when "10"   => return "0100";
          when "11"   => return "1000";
          when others => return "0000";
        end case;
      when HALFWORD =>
        case address is
          when "00"   => return "0011";
          when "10"   => return "1100";
          when others => return "0000";
        end case;
      when others =>
        return "1111";
    end case;
  end function decode_mem_store;

end package body core_pkg;




