#include "htif_rtl_sim.h"

int main(int argc, char** argv)
{
  std::vector<std::string> args(argv + 1, argv + argc);
  htif_rtl_sim_t htif(args);
  
  const int res = htif.connect();
  if(0 == res) {
    printf("Connected to rtl simulator\n");
  }
  
  return htif.run();
}

