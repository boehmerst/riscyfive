library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.func_pkg.all;


entity serial_divider is
  generic (
    width_g    : integer := 32
  );
  port (
    clk_i      : in  std_ulogic;
    reset_n_i  : in  std_ulogic;
    en_i       : in  std_ulogic;
    init_i     : in  std_ulogic;
    start_i    : in  std_ulogic;
    signed_i   : in  std_ulogic;
    dividend_i : in  std_ulogic_vector(width_g-1 downto 0);
    divider_i  : in  std_ulogic_vector(width_g-1 downto 0);
    quotient_o : out std_ulogic_vector(width_g-1 downto 0);
    reminder_o : out std_ulogic_vector(width_g-1 downto 0);
    ready_o    : out std_ulogic    
  );
end entity serial_divider;

architecture rtl of serial_divider is
  type reg_t is record
    start : std_ulogic;
    count : unsigned(log2ceil(width_g) downto 0);
    qr    : std_ulogic_vector(2*width_g-1 downto 0);
  end record reg_t;
  constant dflt_reg_c : reg_t :=(
    start => '0',
    count => (others=>'0'),
    qr    => (others=>'0')
  );
  
  signal r, rin : reg_t;
  
begin
  ------------------------------------------------------------------------------
  -- comb0
  ------------------------------------------------------------------------------
  comb0: process(r, start_i, signed_i, dividend_i, divider_i) is
    variable v        : reg_t;
    variable start    : std_ulogic;
    variable dividend : std_ulogic_vector(width_g-1 downto 0);
    variable divider  : std_ulogic_vector(width_g-1 downto 0);
    variable diff     : std_ulogic_vector(width_g   downto 0);
    variable quotient : std_ulogic_vector(width_g-1 downto 0);
    variable reminder : std_ulogic_vector(width_g-1 downto 0);
  begin
    v := r;
    
    dividend     := dividend_i;
    divider      := divider_i;

    v.start      := start_i;
    start        := start_i and not r.start;
    
    if(signed_i = '1') then
      if(dividend_i(dividend_i'left) = '1') then
        dividend := std_ulogic_vector(unsigned(not dividend_i) + 1);
      end if;
      if(divider_i(divider_i'left) = '1') then
        divider  := std_ulogic_vector(unsigned(not divider_i) + 1);
      end if;
    end if;
    
    diff := std_ulogic_vector(unsigned(r.qr(2*width_g-1 downto width_g-1)) - resize(unsigned(divider), divider'length+1));
    
    if(r.count = 0 and start = '1') then
      v.count := to_unsigned(width_g, v.count'length);
      v.qr    := (v.qr'left downto width_g => '0') & dividend;
    elsif(r.count /= 0) then
      v.count := r.count - 1;
      if(diff(diff'left) = '1') then
        v.qr  := r.qr(2*width_g-2 downto 0) & '0';
      else
        v.qr  := diff(width_g-1 downto 0) & r.qr(width_g-2 downto 0) & '1';
      end if;
    end if;
    
    quotient  := r.qr(width_g-1 downto 0);
    reminder  := r.qr(2*width_g-1 downto width_g);
    
    if(signed_i = '1') then
      if((dividend_i(dividend_i'left) xor divider_i(divider_i'left)) = '1') then
        quotient := std_ulogic_vector(unsigned(not r.qr(width_g-1 downto 0)) + 1);
      end if;
      if(dividend_i(dividend_i'left) = '1') then
        reminder := std_ulogic_vector(unsigned(not r.qr(2*width_g-1 downto width_g)) + 1);
      end if;

    end if;
    
    quotient_o <= quotient;
    reminder_o <= reminder; --TODO: add quotient to get mathematically defined reminder in case of negative results e.g. -11 / 3 = -3, -2 --> -2 + -3 = 1 
    ready_o    <= not v_or(std_ulogic_vector(r.count));
    
    rin <= v;
 end process comb0;
 
 -------------------------------------------------------------------------------
 -- sync0
 -------------------------------------------------------------------------------
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


