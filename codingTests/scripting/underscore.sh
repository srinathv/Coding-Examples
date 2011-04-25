#! /bin/bash

for f in *; do # turn all spaces into underscores
   mv \"$f\" `echo \"$f\" | tr [:space:]  _`
done

