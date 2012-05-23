
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Operators: Operators.o
	${LD} ${LDFLAGS} Operators.o -o Operators

# make this object file

Operators.o: Operators.f90
	${FC} ${FFLAGS} Operators.f90
