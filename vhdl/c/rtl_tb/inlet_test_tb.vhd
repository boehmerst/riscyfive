library std;
use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;

library c;
use c.strings_h.all;
use c.stdio_h.all;

entity inlet_test_tb is
end;

architecture beh of inlet_test_tb is
begin
  process
    variable s, s1, s2: string(1 to 256);
    variable fout:      CFILE;
    variable fin:       CFILE := fopen("pipe2", "r");
    variable buf:       line;
  begin
    printf("--begin test;\n"); 
    fout:=fopen("CON", "w");

    if fin=0 then
      printf("fopen(pipe2, r): could not open pipe2\n");
    else
      for i in 1 to 2 loop
        fscanf(fin, "%s", s);
        fprintf(fout, "vhdl external input: s=%s\n", s);
      end loop;

      fclose(fout);
    end if;
    printf("--end test;\n"); 

    wait;
  end process;
end;

