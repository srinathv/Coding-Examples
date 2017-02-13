
#TAU_PROFILE_FORMAT=merged
#ibrun tau_exec -T knl_17,icpc,papi,mpi,tbb ./mpi_tbb_mm.tauLink.knl.exe

if [ -z "$1" ]
 then
  ibrun -n 2 -o 0 ./mpi_tbb_mm.argsThread.knl.exe
else
  ibrun -n 2 -o 0 ./mpi_tbb_mm.argsThread.knl.exe -threads $1
fi
