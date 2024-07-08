#! /usr/bin/env bash

#allfiles=(*)
#echo "${allfiles[@]}"



#echo "******"
#echo "******"
#echo "******"
#echo "******"
#echo "******"
#echo "******"
#echo "******"
##echo "******"
#echo "******"
mpiAllReducePlusDirs=(mpiAllReduce+*)
echo "${mpiAllReducePlusDirs[@]}"


pat_view --pdf -o tioga_mpiAllreduce.pdf ${mpiAllReducePlusDirs[@]}
