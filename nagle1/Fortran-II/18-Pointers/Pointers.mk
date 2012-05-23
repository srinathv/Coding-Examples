
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Pointers: Pointers.o
	${LD} ${LDFLAGS} Pointers.o -o Pointers

# make this object file

Pointers.o: Pointers.f90
	${FC} ${FFLAGS} Pointers.f90
