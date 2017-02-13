
#TAU_PROFILE_FORMAT=merged
#ibrun tau_exec -T knl_17,icpc,papi,mpi,tbb ./mpi_tbb_mm.tauLink.knl.exe
export LD_LIBRARY_PATH=/opt/apps/papi/5.5.0/lib:$LD_LIBRARY_PATH
if [ -z "$1" ]
 then
   ibrun -n 2 -o 0 tau_exec -T knl_17,icpc,papi,mpi,tbb ./mpi_tbb_mm.argsThread.knl.tau.exe -threads $1
else
   ibrun -n 2 -o 0 tau_exec -T knl_17,icpc,papi,mpi,tbb ./mpi_tbb_mm.argsThread.knl.tau.exe 
fi

