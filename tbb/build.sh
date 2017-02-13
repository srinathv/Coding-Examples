export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-knl_17-icpc-papi-mpi-tbb
#export TAU_MAKEFILE=/Users/srinathv/Repos/NicUOregon/tau2/apple/lib/Makefile.tau-icpc-mpi-tbb

mpiicpc -D__LIKE_GRAVIT mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_tbb_mm.argsThread.knl.exe
