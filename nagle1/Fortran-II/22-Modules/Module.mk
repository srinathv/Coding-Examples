
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Module: Module.o
	${LD} ${LDFLAGS} Module.o -o Module

# make this object file

Module.o: Module.f90
	${FC} ${FFLAGS} Module.f90
