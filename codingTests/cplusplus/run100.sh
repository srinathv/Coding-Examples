#! /usr/bin/env bash


trials=100
        rm -f testVec.out
 for i in `seq 1 ${trials}`;
        do
 #               echo $i
	./${1} >> testVec.out
	sleep 1
        done 
