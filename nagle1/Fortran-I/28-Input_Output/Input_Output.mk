
# use this compiler for the .f90 --> .o step

FC=gfortran -c

# use these options to the compiler

FFLAGS=-std=f2008 -Wall -fcheck=all -fcoarray=single

# use the compiler driver for the .o --> a.out step

LD=gfortran

# use these options to the linker

LDFLAGS=

# make this executable

Input_Output: Input_Output.o
	${LD} ${LDFLAGS} Input_Output.o -o Input_Output

# make this object file

Input_Output.o: Input_Output.f90
	${FC} ${FFLAGS} Input_Output.f90
