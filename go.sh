#!/bin/bash
db=$1
here=$(pwd)

cd ~/local/m8b
bash run.sh ${here}/${db} &
exec ../racket/bin/racket -t go.rkt
