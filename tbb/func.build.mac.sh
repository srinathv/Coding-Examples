#! /usr/local/bin/bash
mpic++ -D__USE_FUNC mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_mm.func.exe
mpic++ -D__USE_TBB -D__USE_FUNC mpi_tbb_mm.cpp -tbb -std=c++0x -o mpi_tbb_mm.func.exe
