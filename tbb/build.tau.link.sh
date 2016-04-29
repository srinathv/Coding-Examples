export TAU_MAKEFILE=/Users/srinathv/Repos/NicUOregon/tau2/apple/lib/Makefile.tau-icpc-mpi-tbb

tau_cxx.sh -optLinkOnly -D__USE_TAU mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_tbb_mm.tauLink
