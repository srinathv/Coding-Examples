
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Reals: Reals.o
	${LD} ${LDFLAGS} Reals.o -o Reals

# make this object file

Reals.o: Reals.f90
	${FC} ${FFLAGS} Reals.f90
