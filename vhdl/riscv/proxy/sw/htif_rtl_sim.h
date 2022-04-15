#pragma once

#include <fesvr/htif.h>
#include <vector>

class htif_rtl_sim_t : public htif_t
{
  public:
    htif_rtl_sim_t(const std::vector<std::string>& args);
    ~htif_rtl_sim_t();

    int connect();

  protected:
    ssize_t read(void* buf, size_t max_size);
    ssize_t write(const void* buf, size_t size);

    size_t chunk_max_size() {return 8;}
    size_t chunk_align() {return 8;}

  private:
    int fin;
    int fout;
};

