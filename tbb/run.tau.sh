
#TAU_PROFILE_FORMAT=merged
#ibrun tau_exec -T knl_17,icpc,papi,mpi,tbb ./mpi_tbb_mm.tauLink.knl.exe
ibrun -n 2 -o 0 tau_exec -T knl_17,icpc,papi,mpi,tbb ./mpi_tbb_mm.argsThread.knl.exe -threads 1
