export PATH=/opt/apps/gcc/4.9.1/bin:$PATH
export LD_LIBRARY_PATH=/opt/apps/gcc/4.9.1/lib:/opt/apps/gcc/4.9.1/lib64:$LD_LIBRARY_PATH
export TAU_MAKEFILE=/work/02463/srinathv/maverick/tau-2.25/x86_64/lib/Makefile.tau-intel15-pdt3_21-TEST-icpc-papi-mpi-pthread-pdt
TAU_OPTIONS="-optPdtCxxParser=cxxparse4101 -optKeepFiles"
tau_cxx.sh simpleClassOneLine.cpp -o simpleClassOneLine
