#! /usr/bin/env bash
export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-intelImpiCuda.aac-icpc-papi-mpi-cupti-pdt

tau_cxx.sh testVec.cpp -o testVec -O2 -fPIC /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a  /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include -lrt
