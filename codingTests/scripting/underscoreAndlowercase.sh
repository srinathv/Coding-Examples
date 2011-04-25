#! /bin/bash

for f in *; do
     file=$(echo $f | tr A-Z a-z | tr ' ' _)
     [ ! -f $file ] &&  mv "$f" $file
done

