#! /usr/bin/env bash




mpicc -o mpi_allreduce -O3 mpi_allreduce.c
mpicxx -o mpiAllReduce -O3 mpiAllReduce.cc


export MPICH_ENV_DISPLAY=1
ml list

PROFILE=`pat_run -w -g mpi`


# need to loop over nodes then ranks
#mpi250 ndoes vs mi300s all needs to be considered.

#pass 1: 1 mpi rank per node
#always do 10 trials per setting

NODE_SET=$(seq 1 16 1) #tiaga
#NODE_SET={1..1000..10} #tiaga

for j in $NODE_SET;
  do
    for i in $(seq 1 10);  
    do	
      flux run -N $j -n $j --exclusive  $PROFILE ./mpiAllReduce 
    done
  done     


