#! /usr/bin/env bash
#
#
#
#
  listOfCce=' cce/14.0.4    cce/15.0.1    cce/16.0.0    cce/16.0.1    cce/17.0.0    cce/17.0.1  cce/17.0.1.1'

 for cce in $listOfCce
 do
	 ml load $cce
	 echo $cce
	 exeName=testAssociate.${cce/\//_}
	 echo $exeName
	 ftn -g -O0 testAssociate.f90 -o $exeName

  done 
