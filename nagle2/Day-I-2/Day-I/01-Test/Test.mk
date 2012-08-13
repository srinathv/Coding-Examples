FC=gfortran -c

FFLAGS=-std=f2008 -Wall -fcheck=all

LD=gfortran

LDFLAGS=

Test: Test.o Test_math_funcs.o Math_funcs.o
	${LD} ${LDFLAGS} Test.o Test_math_funcs.o Math_funcs.o -L${PFUNIT}/lib -lpfunit -o Test

Math_funcs.o: Math_funcs.f90
	${FC} ${FFLAGS} Math_funcs.f90 -o Math_funcs.o

Test_math_funcs.o: Test_math_funcs.f90
	${FC} ${FFLAGS} -I${PFUNIT}/mod -I. Test_math_funcs.f90 -o Test_math_funcs.o

Test.o: Test.f90
	${FC} ${FFLAGS} -I${PFUNIT}/mod -I. Test.f90 -o Test.o

