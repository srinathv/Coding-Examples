
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Logicals: Logicals.o
	${LD} ${LDFLAGS} Logicals.o -o Logicals

# make this object file

Logicals.o: Logicals.f90
	${FC} ${FFLAGS} Logicals.f90
