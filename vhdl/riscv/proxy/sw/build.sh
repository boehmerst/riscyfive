#!/bin/bash

/usr/bin/g++-4.8 -O2 -std=c++11 -c -o htif_rtl_sim.o htif_rtl_sim.cpp -I .
/usr/bin/g++-4.8 -O2 -std=c++11 -c -o fesvr-rtl_sim.o fesvr-rtl_sim.cpp -I .
/usr/bin/g++-4.8 -o fesvr-rtl_sim fesvr-rtl_sim.o htif_rtl_sim.o -lfesvr -L .

