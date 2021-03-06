FC=gfortran-mp-4.7 -c
#FC=ifort -c
#FC=nagfor -c

FCFLAGS=-std=f2008 -Wall -fcheck=all
#FCFLAGS=-std -warn all -check all
#FCFLAGS=-f2008 -w=all -C=all

LD=gfortran-mp-4.7
#LD=ifort
#LD=nagfor

LDFLAGS=${FCFLAGS}

LIBS=

test: f_df.o test_f_df.o
	${LD} ${LDFLAGS} test_f_df.o f_df.o ${LIBS} -o test
	${LD} ${LDFLAGS} test_f_df2d.o f_df2d.o ${LIBS} -o test2d

f_df.o: f_df.f90
	${FC} ${FCFLAGS} f_df.f90

test_f_df.o: f_df.f90
	${FC} ${FCFLAGS} test_f_df.f90

f_df2d.o: f_df2d.f90
	${FC} ${FCFLAGS} f_df2d.f90

test_f_df2d.o: f_df2d.f90
	${FC} ${FCFLAGS} test_f_df2d.f90
