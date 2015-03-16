#! /usr/bin/env bash
BOOST_INCLUDE=-I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include/
export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-intelImpiCuda.aac-icpc-papi-mpi-cupti-pdt
export TAU_OPTIONS="-optLinkOnly -optTauIncludes=${BOOST_INCLUDE}"

tau_cxx.sh testVec.cpp -o testVec_tau -O2 -fPIC -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include/ /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a  /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -lrt
