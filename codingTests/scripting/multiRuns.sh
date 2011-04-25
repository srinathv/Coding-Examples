#! /bin/bash

for (( i=1;i<=5;i++ ))
  do
   echo `mkdir test$i`
   cd test$i
   touch file$i
   cd ..
  done
