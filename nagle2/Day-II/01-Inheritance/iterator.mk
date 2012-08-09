FC=gfortran -c
#FC=ifort -c
#FC=nagfor -c

FCFLAGS=-std=f2008 -Wall -fcheck=all
#FCFLAGS=-std -warn all -check all -heap-arrays
#FCFLAGS=-f2008 -w=all -C=all

LD=gfortran
#LD=ifort
#LD=nagfor

LDFLAGS=${FCFLAGS}

LIBS=

iterator: iterator.o
	${LD} ${LDFLAGS} iterator.o ${LIBS} -o iterator

iterator.o: iterator.f90
	${FC} ${FCFLAGS} iterator.f90
