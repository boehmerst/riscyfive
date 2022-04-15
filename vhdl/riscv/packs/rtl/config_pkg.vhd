library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package config_pkg is

  constant CFG_USE_M_EXT      : boolean  := true;
  
  constant CFG_DMEM_SIZE      : positive := 32;      -- Data memory bus size in 2LOG # elements
  constant CFG_IMEM_SIZE      : positive := 32;      -- Instruction memory bus size in 2LOG # elements  
  constant CFG_BYTE_ORDER     : boolean  := true;    -- Switch between MSB (1, default) and LSB (0) byte order policy

  constant CFG_REG_FORCE_ZERO : boolean  := true;    -- Force data to zero if register address is zero [0,1]
  constant CFG_REG_FWD_WRB    : boolean  := true;    -- Forward writeback to loosen register memory requirements [0,1]
  constant CFG_MEM_FWD_WRB    : boolean  := true;    -- Forward memory result instead of introducing stalls [0,1]

  constant CFG_DMEM_WIDTH     : positive := 32;      -- Data memory width in bits
  constant CFG_IMEM_WIDTH     : positive := 32;      -- Instruction memory width in bits
  constant CFG_GPRF_SIZE      : positive :=  5;      -- General Purpose Register File Size in 2LOG # elements

end package config_pkg;

