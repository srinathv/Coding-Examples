#! /usr/bin/env bash


rank=`mpiRank`
echo ${rank}
echo "This is the rank of this process = ${rank} in step 2 " >> steps_"${rank}".txt
exit
