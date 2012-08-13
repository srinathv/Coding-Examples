#FC=ifort
#
#FCFLAGS=-std -warn all -check all -coarray

FC=gfortran-mp-4.7 

FCFLAGS=-std=f2008 -Wall -fcheck=all -fcoarray=single


mctoy: mctoy.f90
	${FC} ${FCFLAGS} mctoy.f90 ${LIBS} -o mctoy
