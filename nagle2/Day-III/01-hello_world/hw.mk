FC=ifort

FCFLAGS=-std -warn all -check all -coarray

hw: hello_world.f90
	${FC} ${FCFLAGS} hello_world.f90 ${LIBS} -o hw

