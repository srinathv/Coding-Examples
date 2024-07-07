#! /usr/bin/env bash 


flux submit  -N 16 --output=mpiAllreduce_{{id}}.out --error=mpiAllreduce_{{id}.err  ./doMpiAllReduce.sh
