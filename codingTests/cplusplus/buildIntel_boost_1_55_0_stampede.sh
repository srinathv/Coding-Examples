#! /usr/bin/env bash


BASE=testVec.cpp -o testVec -O2 -fPIC  -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include 
BOOST_LIBS=/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -lrt

if ($1 .neq. 
icpc testVec.cpp -o testVec -O2 -fPIC /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a  /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include -lrt


export TAU_MAKEFILE=/work/02463/srinathv/tau2/x86_64/lib/Makefile.tau-intelImpiCuda.aac-icpc-papi-mpi-cupti-pdt
export TAU_OPTIONS="-optLinkOnly -optVerbose"

tau_cxx.sh testVec.cpp -c -O2 -fPIC -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include/ -DUSE_TAU
tau_cxx.sh testVec.o -o testVec_tau -O2 -fPIC -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include/ /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a  /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -lrt

#tau_cxx.sh testVec.cpp -o testVec_tau -O2 -fPIC -I/scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/include/ /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_system.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_timer.a  /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_chrono.a /scratch/02463/srinathv/TEST/Gravity/boost_1_55_0/install/lib/libboost_thread.a -lrt
