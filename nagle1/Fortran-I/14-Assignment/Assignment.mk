
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Assignment: Assignment.o
	${LD} ${LDFLAGS} Assignment.o -o Assignment

# make this object file

Assignment.o: Assignment.f90
	${FC} ${FFLAGS} Assignment.f90
