
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Characters: Characters.o
	${LD} ${LDFLAGS} Characters.o -o Characters

# make this object file

Characters.o: Characters.f90
	${FC} ${FFLAGS} Characters.f90
