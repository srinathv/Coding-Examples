!# env/#!/usr/bin/env bash


mpirun -n 2 tau_exec -T icpc,mpi,tbb ./mpi_tbb_mm.tauLink
