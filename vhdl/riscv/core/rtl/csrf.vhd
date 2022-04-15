library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.config_pkg.all;
use work.core_pkg.all;
use work.func_pkg.all;
use work.inst_pkg.all;
use work.decoupled_io_pkg.all;

entity csrf is
  generic (
    core_id_g      : integer
  );
  port (
    clk_i          : in  std_ulogic;
    reset_n_i      : in  std_ulogic;
    init_i         : in  std_ulogic;
    en_i           : in  std_ulogic;
    csrf_i         : in  csrf_in_t;
    csrf_o         : out csrf_out_t;
    receive_req_i  : in  decoupled_io_req_t;
    receive_rsp_o  : out decoupled_io_rsp_t;
    transmit_req_o : out decoupled_io_req_t;
    transmit_rsp_i : in  decoupled_io_rsp_t
  );
end entity csrf;

architecture rtl of csrf is

  signal hostif_csr_en  : std_ulogic;
  signal hostif_rdata   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
  signal host_csr_addr  : std_ulogic_vector(csr_addr_width_c-1 downto 0);
  signal host_csr_read  : std_ulogic;
  signal host_csr_write : std_ulogic;

begin
  ------------------------------------------------------------------------------
  -- Block: csrf
  ------------------------------------------------------------------------------
  csrf_blocki0: block is
    type reg_t is record
      csr : csr_reg_t; 
    end record reg_t;
    constant dflt_reg_c : reg_t :=(
      csr => dflt_csr_reg_c
    );

    signal r, rin       : reg_t;

  begin
    ----------------------------------------------------------------------------
    -- comb0
    ----------------------------------------------------------------------------
    comb0: process(r, csrf_i) is
      variable v           : reg_t;
      variable csr_addr    : std_ulogic_vector(csr_addr_width_c-1 downto 0);
      variable csr_wr      : std_ulogic;
      variable csr_wdata   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
      variable csr_rdata   : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);
      variable exception   : std_ulogic;
      variable illegal     : std_ulogic;
      variable priviledged : std_ulogic;
      variable cause       : std_ulogic_vector(4 downto 0);
    begin
      v := r;

      --------------------------------------------------------------------------
      -- internal timers and counter
      -- TODO: clock gate will cause counts getting lost!!!
      --------------------------------------------------------------------------
      v.csr.time  := r.csr.time  + 1;
      v.csr.count := r.csr.count + 1;

      if(v.csr.count = r.csr.compare) then
        -- TODO: check if autoclear is intended
        v.csr.status.ip(irq_timer_c) := '1';
      end if;

      --------------------------------------------------------------------------
      -- instruction counter (no need to consider stall cause it gates the clock)
      --------------------------------------------------------------------------
      if(csrf_i.flush = '0' and csrf_i.hazard = '0') then
        v.csr.instret := r.csr.instret + 1;
      end if;

      --------------------------------------------------------------------------
      -- csr handling
      --------------------------------------------------------------------------
      case(csrf_i.ctrl.csr_op) is
        when CSR_S | CSR_C | CSR_W => csr_wr := '1';
        when others                => csr_wr := '0';
      end case;

      csr_addr := csrf_i.addr;

      if(csr_addr = csr_addr_c.fflags) then
        csr_rdata := (csr_rdata'left downto r.csr.fflags'length => '0') & r.csr.fflags;
      elsif(csr_addr = csr_addr_c.frm) then
        csr_rdata := (csr_rdata'left downto r.csr.frm'length => '0') & r.csr.frm;
      elsif(csr_addr = csr_addr_c.fcsr) then
        csr_rdata := (csr_rdata'left downto r.csr.frm'length+r.csr.fflags'length => '0') & r.csr.frm & r.csr.fflags;
      elsif(csr_addr = csr_addr_c.sup0) then
        csr_rdata := r.csr.sup0;
      elsif(csr_addr = csr_addr_c.sup1) then
        csr_rdata := r.csr.sup1;
      elsif(csr_addr = csr_addr_c.epc) then
        csr_rdata := r.csr.epc;
      elsif(csr_addr = csr_addr_c.ptbr) then
        csr_rdata := r.csr.ptbr;
      elsif(csr_addr = csr_addr_c.count) then
        csr_rdata := std_ulogic_vector(r.csr.count);
      elsif(csr_addr = csr_addr_c.compare) then
        csr_rdata := std_ulogic_vector(r.csr.compare);
      elsif(csr_addr = csr_addr_c.evec) then
        csr_rdata := r.csr.evec;
      elsif(csr_addr = csr_addr_c.cause) then
        csr_rdata := (csr_rdata'left downto r.csr.cause'length => '0') & r.csr.cause;
      elsif(csr_addr = csr_addr_c.stats) then
        csr_rdata := (csr_rdata'left downto 1 => '0') & r.csr.stats;
      elsif(csr_addr = csr_addr_c.status) then
        csr_rdata := csr_status(r.csr.status);
      elsif(csr_addr = csr_addr_c.tohost) then
        csr_rdata := r.csr.tohost;
      elsif(csr_addr = csr_addr_c.fromhost) then
        csr_rdata := r.csr.fromhost;
      elsif(csr_addr = csr_addr_c.cycle) then
        csr_rdata := std_ulogic_vector(r.csr.time);
      elsif(csr_addr = csr_addr_c.time) then
        csr_rdata := std_ulogic_vector(r.csr.time);
      elsif(csr_addr = csr_addr_c.hartid) then
        csr_rdata := std_ulogic_vector(to_unsigned(core_id_g, csr_rdata'length));
      else
        csr_rdata := (others=>'0');
      end if;

      case(csrf_i.ctrl.csr_op) is
        when CSR_C  => csr_wdata := csr_rdata and not csrf_i.gprf_data;
        when CSR_S  => csr_wdata := csr_rdata or csrf_i.gprf_data;
        when others => csr_wdata := csrf_i.gprf_data;
      end case;

      if(csrf_i.flush = '0' and csrf_i.ctrl.csr_priv = '0' and csrf_i.ctrl.csr_wr = '1') then
        if(csr_addr = csr_addr_c.fflags) then
          v.csr.fflags   := csr_wdata(v.csr.fflags'left downto 0);
        end if;
        if(csr_addr = csr_addr_c.frm) then
          v.csr.frm      := csr_wdata(v.csr.frm'left downto 0);
        end if;
        if(csr_addr = csr_addr_c.fcsr) then
          v.csr.fflags   := csr_wdata(v.csr.fflags'left downto 0);
          v.csr.frm      := csr_wdata(v.csr.frm'length + v.csr.fflags'length-1 downto v.csr.fflags'length);
        end if;
        if(csr_addr = csr_addr_c.sup0) then
          v.csr.sup0     := csr_wdata;
        end if;
        if(csr_addr = csr_addr_c.sup1) then
          v.csr.sup1     := csr_wdata;
        end if;
        if(csr_addr = csr_addr_c.epc) then
          v.csr.epc      := csr_wdata(v.csr.epc'left downto 0);
        end if;
        if(csr_addr = csr_addr_c.ptbr) then
          v.csr.ptbr     := csr_wdata;
        end if;
        if(csr_addr = csr_addr_c.count) then
          v.csr.count    := unsigned(csr_wdata);
        end if;
        if(csr_addr = csr_addr_c.compare) then
          v.csr.compare  := unsigned(csr_wdata);
          v.csr.status.ip(irq_timer_c) := '0';
        end if;
        if(csr_addr = csr_addr_c.evec) then
          v.csr.evec     := csr_wdata(v.csr.evec'left downto 0);
        end if;
        if(csr_addr = csr_addr_c.stats) then
          v.csr.stats    := csr_wdata(0);
        end if;
        if(csr_addr = csr_addr_c.clear_ipi) then
          v.csr.irq_ipi  := csr_wdata(0);
        end if;
        if(csr_addr = csr_addr_c.status) then
          v.csr.status   := csr_status(csr_wdata);
        end if;
        if(csr_addr = csr_addr_c.tohost) then
          v.csr.tohost   := csr_wdata;
        end if;
        if(csr_addr = csr_addr_c.fromhost) then
          v.csr.fromhost := csr_wdata;
        end if;
        if(csr_addr = csr_addr_c.cycle) then
          v.csr.time     := unsigned(csr_wdata);
        end if;
        if(csr_addr = csr_addr_c.time) then
          v.csr.time     := unsigned(csr_wdata);
        end if;
      end if;

      --------------------------------------------------------------------------
      -- exception handling
      -- TODO: disabled exceptions currently not implemented
      --       irq might get flushed when immediately triggered after a branch
      --       -> will delay irq processing for 2 cycles
      --------------------------------------------------------------------------
      illegal     := (csrf_i.ctrl.sret and r.csr.status.ei) or csrf_i.ctrl.illegal;
      priviledged := (not r.csr.status.s and csrf_i.ctrl.op_priv) or (csrf_i.ctrl.csr_priv and csrf_i.ctrl.csr_en);
      exception   := (csrf_i.ctrl.syscall or illegal or priviledged or csrf_i.irq);

      cause       := exeption_casuse_c.illegal_instruction;
      if(csrf_i.ctrl.syscall = '1') then
        cause     := exeption_casuse_c.syscall;
      elsif(illegal = '1') then
        cause     := exeption_casuse_c.illegal_instruction;
      elsif(priviledged = '1') then
        cause     := exeption_casuse_c.privileged_instruction;
      elsif(csrf_i.irq = '1') then
        cause     := exeption_casuse_c.interrupt(msbset(r.csr.status.ip and r.csr.status.im));
      end if;

      if(exception = '1' and csrf_i.flush = '0')  then
        v.csr.status.s            := '1';
        v.csr.status.ps           := r.csr.status.s;
        v.csr.status.ei           := '0';
        v.csr.status.pei          := r.csr.status.ei;
        v.csr.epc                 := csrf_i.pc;
        v.csr.cause               := cause;
      elsif(csrf_i.ctrl.sret = '1') then
        v.csr.status.s            := r.csr.status.ps;
        v.csr.status.ei           := r.csr.status.pei;
      end if;

      --------------------------------------------------------------------------
      -- drive module output
      --------------------------------------------------------------------------
      csrf_o.csr       <= r.csr;
      csrf_o.rdata     <= csr_rdata;
      csrf_o.exception <= exception;
      hostif_rdata     <= csr_rdata;

      rin <= v;
    end process comb0;

    ----------------------------------------------------------------------------
    -- sync0
    ----------------------------------------------------------------------------
    sync0: process(clk_i, reset_n_i) is
    begin
      if(reset_n_i = '0') then
        r <= dflt_reg_c;
      elsif(rising_edge(clk_i)) then
        if(en_i = '1' or hostif_csr_en = '1') then
          if(init_i = '1') then
            r <= dflt_reg_c;
          else
            r <= rin;
          end if;
        end if;
      end if;
    end process sync0;
  end block csrf_blocki0;

  ------------------------------------------------------------------------------
  -- Block: host interface logic
  ------------------------------------------------------------------------------
  hostif_blocki0: block is
    type reg_t is record
      host_req_valid : std_ulogic;
      host_rsp_valid : std_ulogic;
      receive_rsp    : decoupled_io_rsp_t;
      transmit_req   : decoupled_io_req_t;
    end record reg_t;
    constant dflt_reg_c : reg_t :=(
      host_req_valid => '0',
      host_rsp_valid => '0',
      receive_rsp    => dflt_decoupled_io_rsp_c,
      transmit_req   => dflt_decoupled_io_req_c
    );

    signal cpu_csr_access : std_ulogic;
    signal cpu_busy       : std_ulogic;
    signal csr_rdata      : std_ulogic_vector(CFG_DMEM_WIDTH-1 downto 0);

    signal r, rin         : reg_t;
 
  begin
    -- check if cpu is about to access csr or the clock gate is disabled
    cpu_csr_access <= csrf_i.ctrl.csr_en and not csrf_i.flush and en_i;
    cpu_busy       <= not en_i;
    csr_rdata      <= hostif_rdata;

    ----------------------------------------------------------------------------
    -- comb0
    ----------------------------------------------------------------------------
    comb0: process(r, receive_req_i, transmit_rsp_i, cpu_csr_access, cpu_busy, csr_rdata) is
      variable v              : reg_t;
    begin
      v := r;

      if(io_fire(receive_req_i, r.receive_rsp) = '1') then
        v.host_req_valid         := '1';
        v.transmit_req.bits      := receive_req_i.bits;
      end if;

      -- NOTE: added cpu_busy since we cannot write tho csr whos clock will be gated
      -- TODO: we could think of a more aggressive csr clock gating in future if we'd
      --       implement the counters using another process
      if(r.host_req_valid = '1' and cpu_csr_access = '0' and cpu_busy = '0') then
        v.host_req_valid         := '0';
        v.host_rsp_valid         := '1';
        v.transmit_req.bits.data := csr_rdata;
      end if;

      if(io_fire(r.transmit_req, transmit_rsp_i) = '1') then
        v.host_rsp_valid         := '0';
      end if;

      v.receive_rsp.ready        := not v.host_req_valid and not v.host_rsp_valid;
      v.transmit_req.valid       := v.host_rsp_valid;

      --------------------------------------------------------------------------
      -- drive module output
      --------------------------------------------------------------------------
      receive_rsp_o  <= r.receive_rsp;
      transmit_req_o <= r.transmit_req;

      host_csr_addr  <= r.transmit_req.bits.addr;
      host_csr_read  <= r.host_req_valid;
      host_csr_write <= r.host_req_valid and r.transmit_req.bits.rw and not cpu_csr_access;

      --------------------------------------------------------------------------
      -- drive csrf clock enable
      --------------------------------------------------------------------------
      hostif_csr_en <= r.host_rsp_valid;

      rin           <= v;
    end process comb0;

    ----------------------------------------------------------------------------
    -- sync0
    ----------------------------------------------------------------------------
    sync0: process(clk_i, reset_n_i) is
    begin
      if(reset_n_i = '0') then
        r <= dflt_reg_c;
      elsif(rising_edge(clk_i)) then
        r <= rin;
      end if;
    end process sync0;

  end block hostif_blocki0;

end architecture rtl;

