export LD_LIBRARY_PATH=/opt/apps/papi/5.5.0/lib:$LD_LIBRARY_PATH
tau_exec -T knl_17,icpc,papi,mpi,tbb ./matmul.tau.exe
