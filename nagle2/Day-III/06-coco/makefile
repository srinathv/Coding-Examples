#FC=gfortran -c
#FC=ifort -c
FC=nagfor -c

#FFLAGS=-std=f2008 -Wall -fcheck=all
#FFLAGS=-std -warn all -check all
FFLAGS=-f2008 -w=all -C=all

#LD=gfortran
#LD=ifort
LD=nagfor

coco: coco.o
	${LD} coco.o -o coco

coco.o: coco.f90
	${FC} ${FFLAGS} coco.f90
