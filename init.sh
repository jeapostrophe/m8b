#!/bin/bash
rm -fr root
mkdir root
bash run.sh &
PID=$!
sleep 1
mzscheme -t init.ss
sleep 1
kill $PID
wait $PID