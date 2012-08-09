FC=gfortran -c

FCFLAGS=-std=f2008 -Wall -fcheck=all -pthread

LD=gfortran

LDFLAGS=${FCFLAGS}

LIBS=-lpthread

hw: pthread.o hw.o
	${LD} ${LDFLAGS} hw.o pthread.o ${LIBS} -o hw

pthread.o: pthread.f90
	${FC} ${FCFLAGS} pthread.f90

hw.o: hw.f90
	${FC} ${FCFLAGS} hw.f90
