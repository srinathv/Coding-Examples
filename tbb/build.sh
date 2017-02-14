
mpiicpc -D__USE_TBB -D__LIKE_GRAVIT mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_tbb_mm.argsThread.knl.exe
mpiicpc -D__USE_TBB mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_tbb_mm.knl.exe
mpiicpc mpi_tbb_mm.cpp -std=c++0x -o mpi_mm.knl.exe
icpc matmul.cpp -tbb -o matmul.exe
