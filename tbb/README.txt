README


This directory has tbb examples both serial and parallel with mpi_mm
Are all centered around a matrix matrix multiply.

We want serial example to show
1)serial implementation
2)tbb implemenation with class
3)tbb implementation with lambda expression
4) user thread count specification

Parallel:

1)simple parallel with no tbb
2)collectives in mpi with no tbb
3)with tbb using class
4)with tbb using lambda expression
5)user thread count specification


Profiling:
1)insert simple timers so show different implemenation performance
2)interface with TAU
