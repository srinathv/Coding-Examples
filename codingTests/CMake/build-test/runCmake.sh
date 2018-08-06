#!/bin/bash

if false ; then
  echo "if false"
else
  rm -rf CMake*
  /home/srivad01/software/cmake/882ba7/gnu/7.2/bin/cmake ../.
  if [ $? ]; then
    echo "exit status is NOT 0 "
  else
    echo "exist status is 0"
  fi
fi
