
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Derived_Types: Derived_Types.o
	${LD} ${LDFLAGS} Derived_Types.o -o Derived_Types

# make this object file

Derived_Types.o: Derived_Types.f90
	${FC} ${FFLAGS} Derived_Types.f90
