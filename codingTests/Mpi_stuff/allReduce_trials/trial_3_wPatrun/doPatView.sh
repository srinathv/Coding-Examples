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


pat_view --pdf --pes 10 -o tioga_mpiAllreduce_pe10.pdf ${mpiAllReducePlusDirs[@]}
