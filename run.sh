#!/bin/bash
db=$1
mkdir -p ${db}
exec mongod --dbpath ${db} --noauth --rest
