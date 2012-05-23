
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Generics: Generics.o
	${LD} ${LDFLAGS} Generics.o -o Generics

# make this object file

Generics.o: Generics.f90
	${FC} ${FFLAGS} Generics.f90
