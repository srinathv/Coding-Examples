
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Intrinsics: Intrinsics.o
	${LD} ${LDFLAGS} Intrinsics.o -o Intrinsics

# make this object file

Intrinsics.o: Intrinsics.f90
	${FC} ${FFLAGS} Intrinsics.f90
