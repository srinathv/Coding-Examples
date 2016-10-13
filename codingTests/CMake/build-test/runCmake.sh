#!/bin/bash

if false ; then
  echo "if false"
else
  rm -rf CMake*
  cmake ../.
  if [ $? ]; then
    echo "exit status is NOT 0 "
  else
    echo "exist status is 0"
  fi
fi
