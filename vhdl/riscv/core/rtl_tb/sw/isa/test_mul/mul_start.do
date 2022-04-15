restart -f

#Load the program
mem load -infile sw/isa/test_mul/mul.mem      -format hex /imemi0/ram
mem load -infile sw/isa/test_mul/mul.mem      -format hex /dmemi0/memi0/ram

mem load -infile reg/reg_zero.mem -format hex /corei0/decodei0/gprfi0/a/mem0
mem load -infile reg/reg_zero.mem -format hex /corei0/decodei0/gprfi0/b/mem0
