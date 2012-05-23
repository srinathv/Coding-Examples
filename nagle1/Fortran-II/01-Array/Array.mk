
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Array: Array.o
	${LD} ${LDFLAGS} Array.o -o Array

# make this object file

Array.o: Array.f90
	${FC} ${FFLAGS} Array.f90
