export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-mvapich4_10-icpc-papi-mpi-tbb
source /opt/apps/intel/15/composer_xe_2015.2.164/tbb/bin/tbbvars.sh intel64

tau_cxx.sh -optLinkOnly -D__USE_TAU mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_tbb_mm.tauLink
