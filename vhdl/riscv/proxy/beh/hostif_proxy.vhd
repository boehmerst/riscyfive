library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library std;
use std.textio.all;

library c;
use c.strings_h.all;
use c.stdio_h.all;

library work;
use work.decoupled_io_pkg.all;

entity hostif_proxy is
  generic (
    nr_cores_g        : integer                := 1;
    log2_mem_size_g   : integer range 21 to 31 := 21 -- fesvr requires a minimum of 2 MB
  );
  port (
    clk_i             : in  std_ulogic;
    reset_n_i         : in  std_ulogic;
    sim_done_i        : in  boolean;
    host2mem_req_o    : out decoupled_mem_req_t;
    host2mem_rsp_i    : in  decoupled_mem_rsp_t;
    mem2host_req_i    : in  decoupled_mem_req_t;
    mem2host_rsp_o    : out decoupled_mem_rsp_t;
    host2target_req_o : out decoupled_io_req_t;
    host2target_rsp_i : in  decoupled_io_rsp_t;
    target2host_req_i : in  decoupled_io_req_t;
    target2host_rsp_o : out decoupled_io_rsp_t
  );
end entity hostif_proxy;


architecture beh of hostif_proxy is
  ------------------------------------------------------------------------------
  -- local definitions
  ------------------------------------------------------------------------------
  constant input_file_c      : string := "../bin/proxy_read_inlet";
  constant output_file_c     : string := "../bin/proxy_write_inlet";

  constant max_seqno_c       : integer := 2**8-1;
  constant size_of_header_c  : integer := 8;
  constant htif_data_align_c : integer := 8; 


  ------------------------------------------------------------------------------
  -- local types
  ------------------------------------------------------------------------------
  type htif_cmd_t is ( HTIF_CMD_READ_MEM,          HTIF_CMD_WRITE_MEM, HTIF_CMD_READ_CONTROL_REG,
                       HTIF_CMD_WRITE_CONTROL_REG, HTIF_CMD_ACK,       HTIF_CMD_NACK );

  type header_t is record
    cmd       : htif_cmd_t;
    data_size : integer range 0 to 2**12-1;
    seqno     : integer range 0 to 2**8-1;
    addr      : std_ulogic_vector(39 downto 0);
  end record header_t;

  type packet_t is record
    hdr  : header_t;
    data : std_ulogic_vector(data_width_long_c-1 downto 0);
  end record packet_t;
  constant ack_packet_c : packet_t :=(
    hdr  => (cmd => HTIF_CMD_ACK, data_size => 1, seqno => 0, addr => (others=>'0')),
    data => (others=>'0') 
  );
  constant ack_zero_packet_c : packet_t :=(
    hdr  => (cmd => HTIF_CMD_ACK, data_size => 0, seqno => 0, addr => (others=>'0')),
    data => (others=>'0') 
  );

  type char_file_t is file of character;

  ------------------------------------------------------------------------------
  -- flush output file as Modelsim seems to use buffered I/O
  ------------------------------------------------------------------------------
  procedure flush(file fout :  char_file_t) is
    variable status : file_open_status;
  begin
    file_close(fout);
    file_open(status, fout, output_file_c, write_mode);
    assert status = open_ok severity failure;
  end procedure flush;

  ------------------------------------------------------------------------------
  -- receive packet from emulator (non-blocking)
  ------------------------------------------------------------------------------
  procedure receive(file fin : char_file_t; s : inout string; index : in integer; size : in natural; res : out natural) is
    variable n    : natural;
    variable char : character;
  begin
    n   := 0;
    res := 0;

    if(size = 0) then
      return;
    end if;

    read0: for i in 0 to size-1 loop
      if not endfile(fin) then
        read(fin, char);
        s(index + i) := char;
        n      := n + 1;
      else
        exit read0;
      end if;
    end loop read0; 
    res := n;
  end procedure receive;

  ------------------------------------------------------------------------------
  -- send packet to emulator
  ------------------------------------------------------------------------------
  procedure send(file fout : char_file_t; constant s : in string; size : in natural) is
    variable char : character;
  begin
    if(size = 0) then
      return;
    end if;

    write0: for i in 0 to size-1 loop
      char := s(i+1);
      write(fout, char);
    end loop write0;

    flush(fout);
  end procedure send;

  ------------------------------------------------------------------------------
  -- handshake with emulator
  ------------------------------------------------------------------------------
  impure function handshake(file fin : char_file_t; file fout :  char_file_t) return boolean is
    constant ack_msg : string := "thunder" & nul;
    variable char    : character;
    variable s       : string(1 to 8);
    variable result  : boolean;
  begin
    result := false;
    eos: while true loop
      if not endfile(fin) then
        read(fin, char);
        strcat(s, char);
        if(char = nul) then
          if(strcmp(s, "flash") = 0) then
            send(fout, ack_msg, 8);
            flush(fout);
            result := true;
          end if;
          strcpy(s, nul); -- clear string
          exit eos;
        end if;
      end if;
    end loop eos;
    return result;
  end function handshake;

  ------------------------------------------------------------------------------
  -- get header from string
  ------------------------------------------------------------------------------
  function get_header(s : in string) return header_t is
    variable byte    : unsigned( 7 downto 0);
    variable raw_hdr : unsigned(63 downto 0);
    variable header  : header_t;
  begin
    to_double: for i in 1 to 8 loop
      byte := to_unsigned(character'pos(s(i)),8);
      raw_hdr(8*(i-1)+8-1 downto 8*(i-1)) := byte;
    end loop to_double;

    header.cmd       := htif_cmd_t'val(to_integer(raw_hdr(3 downto 0)));
    header.data_size := to_integer(raw_hdr(15 downto 4));
    header.seqno     := to_integer(raw_hdr(23 downto 16));
    header.addr      := std_ulogic_vector(raw_hdr(63 downto 24));
    
    return header;
  end function get_header;

  ------------------------------------------------------------------------------
  -- get_payload_size
  ------------------------------------------------------------------------------
  function get_payload_size(hdr : header_t) return integer is
  begin
    if(hdr.cmd = HTIF_CMD_READ_MEM or hdr.cmd = HTIF_CMD_READ_CONTROL_REG) then
      return 0;
    else
      return hdr.data_size * htif_data_align_c;
    end if;
  end function get_payload_size;

  ------------------------------------------------------------------------------
  -- get double from string
  ------------------------------------------------------------------------------
  function get_double(s : in string) return std_ulogic_vector is
    variable byte    : unsigned( 7 downto 0);
    variable double  : unsigned(63 downto 0);
  begin
    to_double: for i in 1 to 8 loop
      byte := to_unsigned(character'pos(s(i)),8);
      double(8*(i-1)+8-1 downto 8*(i-1)) := byte;
    end loop to_double;
    return std_ulogic_vector(double);
  end function get_double;

  ------------------------------------------------------------------------------
  -- set_header
  ------------------------------------------------------------------------------
  function set_header(header : header_t) return string is
    variable s       : string(1 to 8);
    variable byte    : unsigned( 7 downto 0);
    variable raw_hdr : unsigned(63 downto 0);
  begin
    raw_hdr( 3 downto 0)  := to_unsigned(htif_cmd_t'pos(header.cmd), 4);
    raw_hdr(15 downto 4)  := to_unsigned(header.data_size, 12);
    raw_hdr(23 downto 16) := to_unsigned(header.seqno, 8);
    raw_hdr(63 downto 24) := unsigned(header.addr);

    to_str: for i in 1 to 8 loop
      byte := raw_hdr(8*(i-1)+8-1 downto 8*(i-1));
      s(i) := character'val(to_integer(byte)); 
    end loop to_str;

    return s;
  end function set_header;

  ------------------------------------------------------------------------------
  -- set_double
  ------------------------------------------------------------------------------
  function set_double(double : std_ulogic_vector(data_width_long_c-1 downto 0)) return string is
    variable s    : string(1 to 8);
    variable byte : unsigned( 7 downto 0);
  begin
    to_str: for i in 1 to 8 loop
      byte := unsigned(double(8*(i-1)+8-1 downto 8*(i-1)));
      s(i) := character'val(to_integer(byte));
    end loop to_str;
    return s;
  end function set_double;

  ------------------------------------------------------------------------------
  -- get_request
  ------------------------------------------------------------------------------
  function get_request(packet : packet_t) return decoupled_io_req_t is
    variable req : decoupled_io_req_t;
  begin
    if(packet.hdr.cmd = HTIF_CMD_WRITE_CONTROL_REG) then
      req.bits.rw := '1';
    else
      req.bits.rw := '0';
    end if;
    req.bits.addr := packet.hdr.addr(req.bits.addr'range);
    req.bits.data := packet.data(req.bits.data'range);
    req.valid     := '1';
    return req;
  end function get_request;

  ------------------------------------------------------------------------------
  -- get_request
  ------------------------------------------------------------------------------
  function get_request(packet : packet_t) return decoupled_mem_req_t is
    variable req : decoupled_mem_req_t;
  begin
    if(packet.hdr.cmd = HTIF_CMD_WRITE_MEM) then
      req.bits.rw := '1';
    else
      req.bits.rw := '0';
    end if;
    req.bits.addr := packet.hdr.addr(req.bits.addr'range);
    req.bits.data := packet.data(req.bits.data'range);
    req.valid     := '1';
    return req;
  end function get_request;

  ------------------------------------------------------------------------------
  -- inc
  ------------------------------------------------------------------------------
  function inc(i : in integer; max : in integer) return integer is
    variable increment : integer;
  begin
    if(max = i) then
      increment := 0;
    else
      increment := i + 1;
    end if;
    return increment;
  end function inc;
begin
  -----------------------------------------------------------------------------
  -- host communication
  -----------------------------------------------------------------------------
  host_req0: process is
    file     fin             : char_file_t open read_mode  is input_file_c;
    file     fout            : char_file_t open write_mode is output_file_c;

    type state_t is (connect, pending_host_command, pending_host_payload, decode_command, target_csr_request, target_mem_request, pending_target);
    variable state           : state_t := connect;

    variable hshake          : boolean;
    variable res             : natural;
    variable index           : natural;
    variable bytes           : natural;
    variable char            : character;
    variable hdr_str         : string(1 to size_of_header_c);
    variable dbl_str         : string(1 to 8);
    variable rx_packet       : packet_t;
    variable tx_packet       : packet_t;
    variable seqno           : integer range 0 to max_seqno_c := 1;

    constant scr_space_c     : std_ulogic_vector(19 downto 0) := x"FFFFF";
    constant pcr_reset_c     : std_ulogic_vector(19 downto 0) := x"0001D";

    variable coreid          : std_ulogic_vector(19 downto 0);
    variable regno           : std_ulogic_vector(19 downto 0);
    variable scr             : std_ulogic_vector(data_width_long_c-1 downto 0);
    variable reset           : std_ulogic := '0';

    variable host2target_req : decoupled_io_req_t   := dflt_decoupled_io_req_c;
    variable target2host_rsp : decoupled_io_rsp_t   := dflt_decoupled_io_rsp_c;
    variable host2mem_req    : decoupled_mem_req_t  := dflt_decoupled_mem_req_c;
    variable mem2host_rsp    : decoupled_mem_rsp_t  := dflt_decoupled_io_rsp_c;
  begin
    wait until clk_i'event and clk_i = '1';

    tick: case state is
      --------------------------------------------------------------------------
      -- connect to hostif server
      --------------------------------------------------------------------------
      when connect =>
        hshake := handshake(fin, fout);
        if(false = hshake) then
          printf("Could not connect to frontend server\n");
          assert false report "error" severity failure;
        end if;

        state := pending_host_command; 
        bytes := size_of_header_c;
        index := 1;

      --------------------------------------------------------------------------
      -- check the host for requests
      --------------------------------------------------------------------------
      when pending_host_command =>
        receive(fin, hdr_str, index, bytes, res);
        bytes := bytes - res;
        index := index + res;

        if(0 = bytes) then
          rx_packet.hdr := get_header(hdr_str);
          state         := pending_host_payload;

          bytes         := get_payload_size(rx_packet.hdr);
          index         := 1;
        end if;

      --------------------------------------------------------------------------
      -- capture payload from host (if any)
      --------------------------------------------------------------------------
      when pending_host_payload =>
        receive(fin, dbl_str, index, bytes, res);
        bytes               := bytes - res;
        index               := index + res;

        if(0 = bytes) then
          rx_packet.data    := get_double(dbl_str);
          state             := decode_command;
        end if;         

      --------------------------------------------------------------------------
      -- decode request
      --------------------------------------------------------------------------
      when decode_command =>
        assert rx_packet.hdr.data_size = 1 severity failure;

        decode0: case rx_packet.hdr.cmd is
          when HTIF_CMD_READ_MEM
            => assert false report "mem read currently not implemented" severity failure;

          when HTIF_CMD_WRITE_MEM =>
            -- send request to target
            host2mem_req := get_request(rx_packet);                
            state        := target_mem_request;

          when HTIF_CMD_READ_CONTROL_REG | HTIF_CMD_WRITE_CONTROL_REG =>
            coreid := rx_packet.hdr.addr(39 downto 20);
            regno  := rx_packet.hdr.addr(19 downto  0);

            -- system control register space is handled here (implies cmd_read command)
            if(coreid = scr_space_c) then
              assert rx_packet.hdr.cmd = HTIF_CMD_READ_CONTROL_REG severity failure;

              case to_integer(unsigned(regno)) is
                when 0      =>
                  scr := std_ulogic_vector(to_unsigned(nr_cores_g, scr'length));
                when 1      =>
                  scr := std_ulogic_vector(to_unsigned(2**log2_mem_size_g, scr'length) srl 20);
                when others =>
                  scr := std_ulogic_vector(to_signed(-1, scr'length));
              end case;

              tx_packet             := ack_packet_c;
              tx_packet.hdr.seqno   := seqno;
              hdr_str               := set_header(tx_packet.hdr);
              dbl_str               := set_double(scr);

              send(fout, hdr_str, size_of_header_c);
              send(fout, dbl_str, 8);

              state                 := pending_host_command;
              bytes                 := size_of_header_c;
              index                 := 1;
            else
              assert unsigned(coreid) < to_unsigned(nr_cores_g, scr'length) severity failure;

              -- reset is handled here (no onchip register)
              if(rx_packet.hdr.cmd = HTIF_CMD_WRITE_CONTROL_REG and regno = pcr_reset_c) then
                tx_packet           := ack_packet_c;
                tx_packet.hdr.seqno := seqno;
                hdr_str             := set_header(tx_packet.hdr);
                dbl_str             := set_double((data_width_long_c-1 downto 1 => '0') & reset);
                reset               := rx_packet.data(0);

                send(fout, hdr_str, size_of_header_c);
                send(fout, dbl_str, 8);

                state               := pending_host_command;
                bytes               := size_of_header_c;
                index               := 1;
              else
                -- send request to target
                host2target_req     := get_request(rx_packet);                
                state               := target_csr_request;
              end if;
            end if;

          when others 
            => null;
        end case decode0;

        seqno := inc(seqno, max_seqno_c);

      --------------------------------------------------------------------------
      -- target request
      --------------------------------------------------------------------------
      when target_csr_request =>
        if(host2target_rsp_i.ready = '1') then
          host2target_req := dflt_decoupled_io_req_c;
          state           := pending_target;
        end if;

      --------------------------------------------------------------------------
      -- memory request
      --------------------------------------------------------------------------
      when target_mem_request =>
        if(host2mem_rsp_i.ready = '1') then
          state                     := pending_target;
          if(host2mem_req.bits.rw = '1') then
            tx_packet               := ack_zero_packet_c;
            tx_packet.hdr.seqno     := seqno;
            hdr_str                 := set_header(tx_packet.hdr);

            send(fout, hdr_str, size_of_header_c);

            state                   := pending_host_command;
            bytes                   := size_of_header_c;
            index                   := 1;
          end if;

          host2mem_req              := dflt_decoupled_mem_req_c;
        end if;

      --------------------------------------------------------------------------
      -- wait for response (implies one pending request at once only)
      --------------------------------------------------------------------------
      when pending_target =>
        if(target2host_req_i.valid = '1') then
          dbl_str   := set_double(std_ulogic_vector(resize(signed(target2host_req_i.bits.data), data_width_long_c)));
        end if;
        if(mem2host_req_i.valid = '1') then
          dbl_str   := set_double(std_ulogic_vector(resize(signed(mem2host_req_i.bits.data), data_width_long_c)));
        end if;

        if(target2host_req_i.valid = '1' or mem2host_req_i.valid = '1') then
          tx_packet           := ack_packet_c;
          tx_packet.hdr.seqno := seqno;

          hdr_str             := set_header(tx_packet.hdr);

          send(fout, hdr_str, size_of_header_c);
          send(fout, dbl_str, 8);

          state               := pending_host_command;
          bytes               := size_of_header_c;
          index               := 1;
        end if;
   
      when others   =>
        assert false report "undefined state should never happen" severity failure;
    end case tick;
    
    ----------------------------------------------------------------------------
    -- drive module output
    ----------------------------------------------------------------------------
    host2target_req_o <= host2target_req;
    target2host_rsp_o <= target2host_rsp;
    host2mem_req_o    <= host2mem_req;
    mem2host_rsp_o    <= mem2host_rsp;

  end process host_req0;

end architecture beh;

