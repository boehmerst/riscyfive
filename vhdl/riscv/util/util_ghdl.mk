# automatic generated ghdl makefile do not edit manually
# library and module name
UTIL_LIB = riscv
UTIL_MOD = util

# compiler and flags
COMP = ghdl -a
ELAB = ghdl -e
UTIL_COMFLAGS = --work=${UTIL_LIB} --workdir=$$DEST_PROJECTS/${UTIL_LIB}/ghdl  -P${DEST_PROJECTS}/c/ghdl -P${DEST_PROJECTS}/riscv/ghdl --std=08 -fexplicit -frelaxed --ieee=synopsys -Wno-hide
UTIL_ELABFLAGS = --work=${UTIL_LIB} --workdir=$$DEST_PROJECTS/${UTIL_LIB}/ghdl  -P${DEST_PROJECTS}/c/ghdl -P${DEST_PROJECTS}/riscv/ghdl --std=08 -fexplicit -frelaxed --ieee=synopsys

# to have an entry point
all: ${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_queue.o ${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_io_pkg.o

# targets to elaborate entities
# targets to analyze files
${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_queue.o: ${GIT_PROJECTS}/vhdl/riscv/util/rtl/decoupled_queue.vhd
	@echo "compile file......." $<
	@$(COMP) $(UTIL_COMFLAGS) $<
	@touch ${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_queue.o

${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_io_pkg.o: ${GIT_PROJECTS}/vhdl/riscv/util/rtl/decoupled_io_pkg.vhd
	@echo "compile file......." $<
	@$(COMP) $(UTIL_COMFLAGS) $<
	@touch ${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_io_pkg.o

# file dependencies
${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_queue.o: ${DEST_PROJECTS}/riscv/ghdl/func_pkg.o ${DEST_PROJECTS}/riscv/ghdl/decoupled_io_pkg.o

${DEST_PROJECTS}/${UTIL_LIB}/ghdl/decoupled_io_pkg.o:

