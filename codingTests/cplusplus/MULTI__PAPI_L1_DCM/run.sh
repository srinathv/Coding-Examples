#! /usr/bin/env bash

export TAU_METRICS='TIME:PAPI_L1_DCM:PAPI_L2_DCM'
export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-intelImpiCuda.aac-icpc-papi-mpi-cupti-pdt
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
./testVec_tau_N1000000
