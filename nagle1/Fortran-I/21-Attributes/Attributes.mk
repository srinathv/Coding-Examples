
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all -fcoarray=single

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Attributes: Attributes.o
	${LD} ${LDFLAGS} Attributes.o -o Attributes

# make this object file

Attributes.o: Attributes.f90
	${FC} ${FFLAGS} Attributes.f90
