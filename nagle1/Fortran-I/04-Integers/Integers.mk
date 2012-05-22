
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Integers: Integers.o
	${LD} ${LDFLAGS} Integers.o -o Integers

# make this object file

Integers.o: Integers.f90
	${FC} ${FFLAGS} Integers.f90
