#! /usr/bin/env bash


SOURCE_DIR=/usr/WS1/srinathv/REPO/Coding-Examples/codingTests/Mpi_stuff

mpicc -o mpi_allreduce -O3 ${SOURCE_DIR}/mpi_allreduce.c
mpicxx -o mpiAllReduce -O3 ${SOURCE_DIR}/mpiAllReduce.cc


export MPICH_ENV_DISPLAY=1
ml list
flux --version 

PROFILE="pat_run -w -g mpi -- "
#TIME="time "
TRIAL=trial_3_wPatrun
mkdir $TRIAL

# need to loop over nodes then ranks
#mpi250 ndoes vs mi300s all needs to be considered.

#pass 1: 1 mpi rank per node
#always do 10 trials per setting

NODE_SET=$(seq 1 16 1) #tiaga
#NODE_SET={1..1000..10} #tiaga

for j in $(seq 1 16)
  do
    export PAT_RT_EXPDIR_BASE=${PWD}/${TRIAL}/node_$j	  
    for i in $(seq 1 10);  
    do	
      output_file=${TRIAL}/mpiAllReduce_nodeNum_${j}_iteration_${i}.out.txt
      date > $output_file
      flux run -N $j -n $j --exclusive -o mpibind=verbose $TIME $PROFILE ./mpiAllReduce  &>> $output_file
      date >> $output_file
    done
  done     


