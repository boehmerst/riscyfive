#include "htif_rtl_sim.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fcntl.h>
#include <unistd.h>

// TODO: make part of command line args
static const char* input_pipe  = "../bin/host_read_inlet";
static const char* output_pipe = "../bin/host_write_inlet";


htif_rtl_sim_t::htif_rtl_sim_t(const std::vector<std::string>& args)
  : htif_t(args)
{
  fin  = open(input_pipe, O_RDONLY);
  fout = open(output_pipe, O_WRONLY);

  // TODO: implemenmt clean error handling
  assert(fin != -1);
  assert(fout != -1);
}


htif_rtl_sim_t::~htif_rtl_sim_t()
{
  close(fin);
  close(fout);
}


int
htif_rtl_sim_t::connect()
{
  const char req_msg[] = "flash";
  const char ack_msg[] = "thunder";
  char       rcv_msg[64];

  const ssize_t wlen = ::write(fout, req_msg, sizeof(req_msg));
  assert(wlen == sizeof(req_msg));

  const ssize_t rlen = ::read(fin, rcv_msg, sizeof(ack_msg));
  assert(rlen == sizeof(ack_msg));
  assert(!strcmp(rcv_msg, ack_msg));

  return 0;
}


ssize_t
htif_rtl_sim_t::read(void* buf, size_t max_size)
{
  ssize_t bytes = sizeof(packet_header_t);
  printf("Incoming read request: %d\n", bytes);

  for(ssize_t i = 0; i < bytes; i++) {
    const int ret = ::read(fin, (char*)buf + i, 1);
    if(ret != 1) {
      printf("error\n");
      return -1;
    }

    if(i == sizeof(packet_header_t)-1) {
      const ssize_t extra_bytes = ((packet_header_t*)buf)->get_payload_size();
      printf("\nwaiting for %d additional bytes\n", extra_bytes);
      bytes += extra_bytes;
    }

    printf("0x%x ", *((char*)buf + i) & 0xff);
  }

  printf("\n");

  assert(bytes <= max_size);
  return bytes;
}


ssize_t
htif_rtl_sim_t::write(const void* buf, size_t size)
{
  printf("Incoming write request: 0x%0llx\n", *(uint64_t*)(buf));
  return ::write(fout, buf, size);
}

