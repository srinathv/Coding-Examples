
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all -fcoarray=single

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Executables: Executables.o
	${LD} ${LDFLAGS} Executables.o -o Executables

# make this object file

Executables.o: Executables.f90
	${FC} ${FFLAGS} Executables.f90
