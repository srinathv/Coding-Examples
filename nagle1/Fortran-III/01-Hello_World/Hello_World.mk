
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all -fcoarray=single

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Hello_World: Hello_World.o
	${LD} ${LDFLAGS} Hello_World.o -o Hello_World

# make this object file

Hello_World.o: Hello_World.f90
	${FC} ${FFLAGS} Hello_World.f90
