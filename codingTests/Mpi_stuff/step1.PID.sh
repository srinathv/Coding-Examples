#! /usr/bin/env bash

# this is to test to write to a file named by PID

echo "This program shows the PIDs of each process"

echo $$, " is the PID of a rank" > steps_$$.txt
