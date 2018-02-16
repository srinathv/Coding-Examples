#! /usr/bin/env bash


FC=gfortran
TYPE=gnu
SRC=single_task.F

BASIC_FLAGS='-g -O3 -fopenmp'

$FC $SRC $BASIC_FLAGS -o no_single_task.${TYPE}.exe
$FC $SRC $BASIC_FLAGS -DSINGLE -o single_task.${TYPE}.exe
$FC $SRC $BASIC_FLAGS -DSINGLE -DMORETASK -o single_moretask.${TYPE}.exe
$FC $SRC $BASIC_FLAGS -DTASKLOOP -o no_single_taskloop.${TYPE}.exe
$FC $SRC $BASIC_FLAGS -DSINGLE -DTASKLOOP -o single_taskloop.${TYPE}.exe

echo " OMP_NUM_THREADS=5 ./no_single_task.${TYPE}.exe"
OMP_NUM_THREADS=5 ./no_single_task.${TYPE}.exe
echo " OMP_NUM_THREADS=5 ./single_task.${TYPE}.exe"
OMP_NUM_THREADS=5 ./single_task.${TYPE}.exe
echo " OMP_NUM_THREADS=5 ./single_moretask.${TYPE}.exe"
OMP_NUM_THREADS=5 ./single_moretask.${TYPE}.exe
echo " OMP_NUM_THREADS=5 ./no_single_taskloop.${TYPE}.exe"
OMP_NUM_THREADS=5 ./no_single_taskloop.${TYPE}.exe
echo " OMP_NUM_THREADS=5 ./single_taskloop.${TYPE}.exe"
OMP_NUM_THREADS=5 ./single_taskloop.${TYPE}.exe




