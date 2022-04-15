#!/bin/bash

../bin/inlet -v -a ../bin/host_write_inlet ../bin/proxy_read_inlet &
../bin/inlet -v -a ../bin/proxy_write_inlet ../bin/host_read_inlet &
#../bin/emulator &


