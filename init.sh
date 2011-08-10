#!/bin/bash
rm -fr root
mkdir root
bash run.sh &
PID=$!
sleep 1
../racket/bin/racket -t init.rkt
sleep 1
kill $PID
wait $PID
