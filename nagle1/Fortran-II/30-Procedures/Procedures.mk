
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Procedures: Procedures.o
	${LD} ${LDFLAGS} Procedures.o -o Procedures

# make this object file

Procedures.o: Procedures.f90
	${FC} ${FFLAGS} Procedures.f90
