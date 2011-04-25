#! /bin/bash

for f in *.mp3; do
    mv "$f" "0$f"
done
