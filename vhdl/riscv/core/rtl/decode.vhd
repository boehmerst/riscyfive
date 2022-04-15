library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.core_pkg.all;
use work.inst_pkg.all;
use work.func_pkg.all;

entity decode is
  generic (
    use_m_ext_g   : boolean;
    use_dbg_g     : boolean
  );
  port (
    clk_i         : in  std_ulogic;
    reset_n_i     : in  std_ulogic;
    init_i        : in  std_ulogic;
    en_i          : in  std_ulogic;
    decode_i      : in  decode_in_t;
    decode_o      : out decode_out_t;
    decode_comb_o : out decode_comb_out_t;
    gprf_o        : out gprf_out_t
  );
end entity decode;

architecture rtl of decode is

  type reg_t is record
    decode         : decode_out_t;
    pc             : std_ulogic_vector(CFG_IMEM_SIZE-1 downto 0);
    inst           : std_ulogic_vector(CFG_IMEM_WIDTH-1 downto 0);
    start          : std_ulogic;
    irq            : std_ulogic;
    irq_en         : std_ulogic;
    irq_delay      : std_ulogic;
    hazard         : std_ulogic;
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    decode         => dflt_decode_out_c,
    pc             => (others=>'0'),
    inst           => (others=>'0'),
    start          => '0',
    irq            => '0',
    irq_en         => '1',
    irq_delay      => '0',
    hazard         => '0'
  );

  signal gprf      : gprf_in_t;
  signal wb_data_d : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0); 
  signal r, rin    : reg_t;

begin
  ------------------------------------------------------------------------------
  -- Scalar Unit Register File
  ------------------------------------------------------------------------------
  gprf.ena   <= en_i;
  gprf.adr_a <= rin.decode.reg_a;
  gprf.adr_b <= rin.decode.reg_b;
  gprf.dat_w <= wb_data_d;
  gprf.adr_w <= decode_i.ctrl_wrb.reg_d;
  gprf.wre   <= decode_i.ctrl_wrb.reg_write;

  gprfi0 : entity work.gprf
    generic map (
      dmem_width_g => CFG_DMEM_WIDTH,
      gprf_size_g  => CFG_GPRF_SIZE
    )
    port map (
      clk_i  => clk_i,
      gprf_o => gprf_o,
      gprf_i => gprf
    );

  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, decode_i) is
    variable v           : reg_t;
    variable wb_result   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable instruction : std_ulogic_vector(CFG_IMEM_WIDTH-1 downto 0);
    variable pc          : std_ulogic_vector(CFG_IMEM_SIZE-1  downto 0);
    variable reg_a       : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    variable reg_b       : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    variable reg_d       : std_ulogic_vector(CFG_GPRF_SIZE-1  downto 0);
    variable opcode      : std_ulogic_vector(6 downto 0);
    variable inst_rtype  : inst_rtype_t;
    variable inst_itype  : inst_itype_t;
    variable inst_stype  : inst_stype_t;
    variable inst_utype  : inst_utype_t;
    variable inst_iimm   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable inst_simm   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable inst_bimm   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable inst_uimm   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable inst_jimm   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
    variable irq_masked  : std_ulogic_vector(7 downto 0);
    variable irq_fire    : std_ulogic;
  begin
    v       := r;
    v.start := '1';

    ----------------------------------------------------------------------------
    -- TODO: adjust according to riscv supervisor specification
    ----------------------------------------------------------------------------
    irq_masked := decode_i.irq.pending and decode_i.irq.mask;
    irq_fire   := v_or(irq_masked) and decode_i.irq.enable;

    v.irq_delay := '0';
    if(irq_fire = '1') then
      v.irq     := '1';
    end if;

    ----------------------------------------------------------------------------
    -- latch instruction
    ----------------------------------------------------------------------------
    v.pc                         := decode_i.pc;
    v.inst                       := decode_i.inst;
    
    ----------------------------------------------------------------------------
    -- write-back multiplexor
    ----------------------------------------------------------------------------
    if(decode_i.ctrl_mem_wrb.mem_read = '1') then
      wb_result                  := align_mem_load(decode_i.mem_result, decode_i.ctrl_mem_wrb.transfer_size, decode_i.alu_result(1 downto 0), decode_i.ctrl_mem_wrb.zero_extend);
    else
      wb_result                  := decode_i.alu_result;
    end if;

    ----------------------------------------------------------------------------
    -- forward bypass logic
    ----------------------------------------------------------------------------
    if(CFG_REG_FWD_WRB = true) then
      v.decode.fwd_dec_result     := wb_result;
      v.decode.fwd_dec            := decode_i.ctrl_wrb;
    else
      v.decode.fwd_dec_result     := (others=>'0');
      v.decode.fwd_dec.reg_d      := (others=>'0');
      v.decode.fwd_dec.reg_write  := '0';
    end if;

    ----------------------------------------------------------------------------
    -- source and destination decoding
    ----------------------------------------------------------------------------
    reg_a  := get_rega(decode_i.inst);
    reg_b  := get_regb(decode_i.inst);
    reg_d  := get_regd(decode_i.inst);

    ----------------------------------------------------------------------------
    -- hazard processing -> works pessimistic in case of none rtype instructions
    -- to follow memory load instructions
    ----------------------------------------------------------------------------
    if((not decode_i.flush_id and r.decode.ctrl_mem.mem_read and (compare(reg_a, r.decode.ctrl_wrb.reg_d) or compare(reg_b, r.decode.ctrl_wrb.reg_d))) = '1') then
      pc                    := (others => '0');    
      instruction           := (others => '0');
      v.hazard   := '1';
    elsif(CFG_MEM_FWD_WRB = false and (not decode_i.flush_id and r.decode.ctrl_mem.mem_read and compare(reg_d, r.decode.ctrl_wrb.reg_d)) = '1') then
      pc                    := (others => '0');    
      instruction           := (others => '0');
      v.hazard   := '1';
    elsif(r.decode.stall = '1') then
      pc                    := r.pc;
      instruction           := r.inst;
      v.decode.stall        := '0';
    elsif(r.hazard = '1') then
      pc                    := r.pc;
      instruction           := r.inst;
      v.hazard   := '0';
    else
      pc                    := decode_i.pc;
      instruction           := decode_i.inst;
      v.hazard   := '0';
    end if;

    ----------------------------------------------------------------------------
    -- instruction and immediate type generation
    ----------------------------------------------------------------------------
    inst_rtype := get_inst(instruction);
    inst_itype := get_inst(instruction);
    inst_stype := get_inst(instruction);
    inst_utype := get_inst(instruction);

    inst_iimm  := get_iimm(instruction);
    inst_simm  := get_simm(instruction);
    inst_bimm  := get_bimm(instruction);
    inst_uimm  := get_uimm(instruction);
    inst_jimm  := get_jimm(instruction);

    ----------------------------------------------------------------------------
    -- default assignment for scalar unit
    ----------------------------------------------------------------------------
    v.decode.pc                 := pc;
    v.decode.ctrl_wrb.reg_d     := inst_rtype.rd;
    v.decode.reg_a              := inst_rtype.rs1;
    v.decode.reg_b              := inst_rtype.rs2;

    v.decode.imm                := (others=>'0');
    v.decode.ctrl_ex            := dflt_ctrl_execution_c;
    v.decode.ctrl_mem           := dflt_ctrl_memory_c;
    v.decode.ctrl_wrb.reg_write := '0';
    
    opcode                      := get_opcode(instruction);

    if(r.start = '0') then
      --------------------------------------------------------------------------
      -- supress decoding during the very first (uncompleted) fetch phase
      --------------------------------------------------------------------------
      v.decode                  := dflt_decode_out_c;

    elsif(v.irq = '1' and r.irq_delay = '0' and decode_i.flush_id = '0' and v.hazard = '0') then
      --------------------------------------------------------------------------
      -- interrupt processing
      --------------------------------------------------------------------------
      v.irq                     := '0';
      v.decode.ctrl_ex.irq      := '1';

    elsif(decode_i.flush_id = '1' or v.hazard = '1') then
      --------------------------------------------------------------------------
      -- hazard or flush_id (just to ease debugging remove to save logic)
      --------------------------------------------------------------------------
      if(use_dbg_g = true) then
        v.decode.pc             := (others=>'0');
        v.decode.ctrl_wrb.reg_d := (others=>'0');
        v.decode.reg_a          := (others=>'0');
        v.decode.reg_b          := (others=>'0');
        v.decode.imm            := (others=>'0');
      end if;

    elsif(compare(opcode(6 downto 2), inst_load_c)     = '1') then
      --------------------------------------------------------------------------
      --  load is encoded as I-Type instruction
      --------------------------------------------------------------------------
      v.decode.imm                  := inst_iimm;
      v.decode.ctrl_ex.alu_op       := ALU_ADD;
      v.decode.ctrl_ex.alu_src_a    := ALU_SRC_REGA;
      v.decode.ctrl_ex.alu_src_b    := ALU_SRC_IMM;
      v.decode.ctrl_mem.mem_write   := '0';
      v.decode.ctrl_mem.mem_read    := '1';
      v.decode.ctrl_mem.zero_extend := inst_itype.func3(2); --TODO: modify align_mem_load to support sign extention
      v.decode.ctrl_wrb.reg_write   := is_not_zero(v.decode.ctrl_wrb.reg_d);

      case inst_itype.func3(1 downto 0) is
        when "00"   => v.decode.ctrl_mem.transfer_size := BYTE;
        when "01"   => v.decode.ctrl_mem.transfer_size := HALFWORD;
        when others => v.decode.ctrl_mem.transfer_size := WORD;
      end case;

    elsif(compare(opcode(6 downto 2), inst_store_c)    = '1') then
      --------------------------------------------------------------------------
      -- store is S-Type instruction
      --------------------------------------------------------------------------
      v.decode.imm                  := inst_simm;
      v.decode.ctrl_ex.alu_op       := ALU_ADD;
      v.decode.ctrl_ex.alu_src_a    := ALU_SRC_REGA;
      v.decode.ctrl_ex.alu_src_b    := ALU_SRC_IMM;
      v.decode.ctrl_mem.mem_write   := '1';
      v.decode.ctrl_mem.mem_read    := '0';

      case inst_itype.func3(1 downto 0) is
        when "00"   => v.decode.ctrl_mem.transfer_size := BYTE;
        when "01"   => v.decode.ctrl_mem.transfer_size := HALFWORD;
        when others => v.decode.ctrl_mem.transfer_size := WORD;
      end case;

    elsif(compare(opcode(6 downto 2), inst_op_c) = '1' and inst_rtype.func7(0) = '0') then
      --------------------------------------------------------------------------
      -- integer computational instructions (register-register)
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write := is_not_zero(v.decode.ctrl_wrb.reg_d);

      case inst_rtype.func3 is
        when op_add_c  => v.decode.ctrl_ex.alu_op      := ALU_ADD;
                          if(inst_rtype.func7(5) = '1') then
                            v.decode.ctrl_ex.alu_src_b := ALU_SRC_NOT_REGB;
                            v.decode.ctrl_ex.carry     := CARRY_ONE;
                          end if;

        when op_sll_c  => v.decode.ctrl_ex.alu_op      := ALU_SHIFT_LEFT;

        when op_slt_c  => v.decode.ctrl_ex.alu_op      := ALU_COMP;
                          v.decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_REGB;
                          v.decode.ctrl_ex.carry       := CARRY_ONE;

        when op_sltu_c => v.decode.ctrl_ex.alu_op      := ALU_UCOMP;
                          v.decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_REGB;
                          v.decode.ctrl_ex.carry       := CARRY_ONE;
        
        when op_sr_c   => v.decode.ctrl_ex.alu_op      := ALU_SHIFT_RIGHT;
                          if(inst_rtype.func7(5) = '1') then
                            v.decode.ctrl_ex.carry     := CARRY_ARITH;
                          end if;

        when op_or_c   => v.decode.ctrl_ex.alu_op      := ALU_OR;
        when op_xor_c  => v.decode.ctrl_ex.alu_op      := ALU_XOR;
        when op_and_c  => v.decode.ctrl_ex.alu_op      := ALU_AND;

        when others    => null;
      end case;

    elsif(compare(opcode(6 downto 2), inst_op_imm_c)   = '1') then
      --------------------------------------------------------------------------
      -- integer computational instructions (immediate-register)
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write := is_not_zero(v.decode.ctrl_wrb.reg_d);
      v.decode.ctrl_ex.alu_src_b  := ALU_SRC_IMM;
      v.decode.imm                := inst_iimm;

      case inst_itype.func3 is
        when op_add_c  => v.decode.ctrl_ex.alu_op      := ALU_ADD;
        when op_sll_c  => v.decode.ctrl_ex.alu_op      := ALU_SHIFT_LEFT;
                          v.decode.ctrl_ex.csr.illegal := inst_iimm(5);

        when op_slt_c  => v.decode.ctrl_ex.alu_op      := ALU_COMP;
                          v.decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_IMM;
                          v.decode.ctrl_ex.carry       := CARRY_ONE;

        when op_sltu_c => v.decode.ctrl_ex.alu_op      := ALU_UCOMP;
                          v.decode.ctrl_ex.alu_src_b   := ALU_SRC_NOT_IMM;
                          v.decode.ctrl_ex.carry       := CARRY_ONE;

        when op_sr_c   => v.decode.ctrl_ex.alu_op      := ALU_SHIFT_RIGHT;
                          v.decode.ctrl_ex.csr.illegal := inst_iimm(5);
                          if(inst_rtype.func7(5) = '1') then
                            v.decode.ctrl_ex.carry     := CARRY_ARITH;
                          end if;

        when op_or_c   => v.decode.ctrl_ex.alu_op      := ALU_OR;
        when op_xor_c  => v.decode.ctrl_ex.alu_op      := ALU_XOR;
        when op_and_c  => v.decode.ctrl_ex.alu_op      := ALU_AND;
        when others    => null;
      end case;

    elsif(compare(opcode(6 downto 2), inst_lui_c)      = '1') then
      --------------------------------------------------------------------------
      -- load upper immediate (LSBs set to zero)
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write := is_not_zero(v.decode.ctrl_wrb.reg_d);
      v.decode.ctrl_ex.alu_src_a  := ALU_SRC_ZERO;
      v.decode.ctrl_ex.alu_src_b  := ALU_SRC_IMM;
      v.decode.imm                := inst_uimm;

    elsif(compare(opcode(6 downto 2), inst_auipc_c)    = '1') then
      --------------------------------------------------------------------------
      -- add upper immediate to PC
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write := is_not_zero(v.decode.ctrl_wrb.reg_d);
      v.decode.ctrl_ex.alu_src_a  := ALU_SRC_PC;
      v.decode.ctrl_ex.alu_src_b  := ALU_SRC_IMM;
      v.decode.imm                := inst_uimm;

    elsif(compare(opcode(6 downto 2), inst_jal_c)      = '1') then
      --------------------------------------------------------------------------
      -- jump and link (unconditional branch)
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write  := is_not_zero(v.decode.ctrl_wrb.reg_d);
      v.decode.ctrl_ex.alu_src_a   := ALU_SRC_PC;
      v.decode.ctrl_ex.alu_src_b   := ALU_SRC_IMM;
      v.decode.ctrl_ex.branch_cond := JAL;
      v.decode.imm                 := inst_jimm;

    elsif(compare(opcode(6 downto 2), inst_jalr_c)     = '1') then
      --------------------------------------------------------------------------
      -- jump and link register (unconditional indirect branch)
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write  := is_not_zero(v.decode.ctrl_wrb.reg_d);
      v.decode.ctrl_ex.alu_src_b   := ALU_SRC_IMM;
      v.decode.ctrl_ex.branch_cond := JALR;
      v.decode.imm                 := inst_iimm;

    elsif(compare(opcode(6 downto 2), inst_branch_c)   = '1') then
      --------------------------------------------------------------------------
      -- conditional branches 
      --------------------------------------------------------------------------
      v.decode.imm               := inst_bimm;
      v.decode.ctrl_ex.alu_src_b := ALU_SRC_NOT_REGB;
      v.decode.ctrl_ex.carry     := CARRY_ONE;
     
      case inst_stype.func3 is
        when cond_beq_c  => v.decode.ctrl_ex.branch_cond := BEQ;
        when cond_bne_c  => v.decode.ctrl_ex.branch_cond := BNE;
        when cond_blt_c  => v.decode.ctrl_ex.branch_cond := BLT;
        when cond_bge_c  => v.decode.ctrl_ex.branch_cond := BGE;
        when cond_bltu_c => v.decode.ctrl_ex.branch_cond := BLTU;
        when cond_bgeu_c => v.decode.ctrl_ex.branch_cond := BGEU;
        when others      => v.decode.ctrl_ex.branch_cond := JAL;
      end case;

    elsif(compare(opcode(6 downto 2), inst_misc_mem_c) = '1') then
      --------------------------------------------------------------------------
      -- memory fence instructions (currently not supported)
      --------------------------------------------------------------------------    
    elsif(compare(opcode(6 downto 2), inst_system_c)   = '1') then
      --------------------------------------------------------------------------
      -- system instruction (could be emulated as traps)
      --------------------------------------------------------------------------
      v.decode.imm                := inst_iimm;
      v.decode.ctrl_wrb.reg_write := is_not_zero(v.decode.ctrl_wrb.reg_d);

      case inst_itype.func3 is
        when "000"  => -- SBREAK instruction not considered
                       v.decode.ctrl_ex.csr.syscall   := not inst_itype.imm12(11); -- SCALL

                       if(inst_itype.imm12(11) = '1') then
                         v.decode.ctrl_ex.csr.sret    := '1'; -- SRET
                         v.decode.ctrl_ex.csr.op_priv := '1';
                       end if;

        when "001"  => -- CSRRW 
                       v.decode.ctrl_ex.csr.csr_op    := CSR_W;
                       v.decode.ctrl_ex.csr.csr_wr    := '1';
                       v.decode.ctrl_ex.csr.csr_en    := '1';
                       v.decode.ctrl_ex.csr.csr_priv  := csr_priviledged(v.decode.imm(csr_addr_width_c-1 downto csr_addr_width_c-4), 
                                                                         v.decode.ctrl_ex.csr.csr_wr, decode_i.mode);
        when "010"  => -- CSRRS
                       v.decode.ctrl_ex.csr.csr_op    := CSR_S;
                       v.decode.ctrl_ex.csr.csr_wr    := is_not_zero(v.decode.reg_a);
                       v.decode.ctrl_ex.csr.csr_en    := '1';
                       v.decode.ctrl_ex.csr.csr_priv  := csr_priviledged(v.decode.imm(csr_addr_width_c-1 downto csr_addr_width_c-4), 
                                                                         v.decode.ctrl_ex.csr.csr_wr, decode_i.mode);
        when "011"  => -- CSRRC
                       v.decode.ctrl_ex.csr.csr_op    := CSR_C;
                       v.decode.ctrl_ex.csr.csr_wr    := is_not_zero(v.decode.reg_a);
                       v.decode.ctrl_ex.csr.csr_en    := '1';
                       v.decode.ctrl_ex.csr.csr_priv  := csr_priviledged(v.decode.imm(csr_addr_width_c-1 downto csr_addr_width_c-4), 
                                                                         v.decode.ctrl_ex.csr.csr_wr, decode_i.mode);
        when "101"  => -- CSRRWI
                       v.decode.ctrl_ex.alu_src_a     := ALU_SRC_ZIMM;
                       v.decode.ctrl_ex.csr.csr_op    := CSR_W;
                       v.decode.ctrl_ex.csr.csr_wr    := '1';
                       v.decode.ctrl_ex.csr.csr_en    := '1';
                       v.decode.ctrl_ex.csr.csr_priv  := csr_priviledged(v.decode.imm(csr_addr_width_c-1 downto csr_addr_width_c-4), 
                                                                         v.decode.ctrl_ex.csr.csr_wr, decode_i.mode);
        when "110"  => -- CSRRSI
                       v.decode.ctrl_ex.alu_src_a     := ALU_SRC_ZIMM;
                       v.decode.ctrl_ex.csr.csr_op    := CSR_S;
                       v.decode.ctrl_ex.csr.csr_wr    := is_not_zero(v.decode.reg_a);
                       v.decode.ctrl_ex.csr.csr_en    := '1';
                       v.decode.ctrl_ex.csr.csr_priv  := csr_priviledged(v.decode.imm(csr_addr_width_c-1 downto csr_addr_width_c-4), 
                                                                         v.decode.ctrl_ex.csr.csr_wr, decode_i.mode);
        when "111"  => -- CSRRCI
                       v.decode.ctrl_ex.alu_src_a     := ALU_SRC_ZIMM;
                       v.decode.ctrl_ex.csr.csr_op    := CSR_C;
                       v.decode.ctrl_ex.csr.csr_wr    := is_not_zero(v.decode.reg_a);
                       v.decode.ctrl_ex.csr.csr_en    := '1';
                       v.decode.ctrl_ex.csr.csr_priv  := csr_priviledged(v.decode.imm(csr_addr_width_c-1 downto csr_addr_width_c-4), 
                                                                        v.decode.ctrl_ex.csr.csr_wr, decode_i.mode);
        when others => null;
      end case;

    elsif(use_m_ext_g and compare(opcode(6 downto 2), inst_op_c) = '1' and inst_rtype.func7(0) = '1') then
      --------------------------------------------------------------------------
      -- 32 bit M extention
      --------------------------------------------------------------------------
      v.decode.ctrl_wrb.reg_write := is_not_zero(v.decode.ctrl_wrb.reg_d);

      -- serial divider requires stalling the pipeline
      if(inst_rtype.func3(2) = '1') then
        if(r.decode.stall = '0') then
          v.decode.stall          := '1';
        end if;
      end if;

      case inst_rtype.func3 is
        when op_mul_c    => v.decode.ctrl_ex.alu_op := ALU_MUL;
                            v.decode.ctrl_ex.mul_op := MUL_MULU;

        when op_mulh_c   => v.decode.ctrl_ex.alu_op := ALU_MULH;
                            v.decode.ctrl_ex.mul_op := MUL_MULS;

        when op_mulhsu_c => v.decode.ctrl_ex.alu_op := ALU_MULH;
                            v.decode.ctrl_ex.mul_op := MUL_MULSU;

        when op_mulhu_c  => v.decode.ctrl_ex.alu_op := ALU_MULH;
                            v.decode.ctrl_ex.mul_op := MUL_MULU;

        when op_div_c    => v.decode.ctrl_ex.alu_op := ALU_DIV;
                            v.decode.ctrl_ex.div_op := DIV_DIVS;

        when op_divu_c   => v.decode.ctrl_ex.alu_op := ALU_DIV;
                            v.decode.ctrl_ex.div_op := DIV_DIVU;

        when op_rem_c    => v.decode.ctrl_ex.alu_op := ALU_REM;
                            v.decode.ctrl_ex.div_op := DIV_DIVS;

        when op_remu_c   => v.decode.ctrl_ex.alu_op := ALU_REM;
                            v.decode.ctrl_ex.div_op := DIV_DIVU;

        when others      => null;
      end case;
    else
      v.decode.ctrl_ex.csr.illegal := '1';
    end if;

    ----------------------------------------------------------------------------
    -- latch current instruction that causes the stall
    ----------------------------------------------------------------------------
    if(v.decode.stall = '1') then
      v.pc   := pc;
      v.inst := instruction;
    end if;

    ----------------------------------------------------------------------------
    -- drive module output and internals
    ----------------------------------------------------------------------------
    wb_data_d     <= wb_result;
    decode_o      <= r.decode;
    decode_comb_o <= (hazard => v.hazard, stall => v.decode.stall);

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
