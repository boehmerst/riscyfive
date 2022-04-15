#!/bin/bash

riscv-gcc -fpic -m32 -nostdlib -nostartfiles -Wa,-march=RVIMAFDXhwacha -I ../env/p -I macros/scalar/ -T ../env/p/link.ld -o $1.elf rv32ui/$1.S
riscv-objdump -h -S -C $1.elf > $1.lss
riscv-nm -n $1.elf > $1.sym
riscv-objcopy -O binary $1.elf $1.bin
../../../../../../../../software/util/bin2mem < $1.bin > $1.mem

echo "restart -f

#Load the program
mem load -infile sw/isa/test_$1/$1.mem      -format hex /imemi0/ram
mem load -infile sw/isa/test_$1/$1.mem      -format hex /dmemi0/memi0/ram

mem load -infile reg/reg_zero.mem -format hex /corei0/decodei0/gprfi0/a/mem0
mem load -infile reg/reg_zero.mem -format hex /corei0/decodei0/gprfi0/b/mem0" > $1_start.do

mkdir -p test_$1
mv $1* test_$1

