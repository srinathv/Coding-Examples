
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Array_Intrinsic: Array_Intrinsic.o
	${LD} ${LDFLAGS} Array_Intrinsic.o -o Array_Intrinsic

# make this object file

Array_Intrinsic.o: Array_Intrinsic.f90
	${FC} ${FFLAGS} Array_Intrinsic.f90
