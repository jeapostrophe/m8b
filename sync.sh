#!/bin/sh
rsync -av --progress . m8b:local/m8b/ --exclude 'admissions*' --exclude '*pem' --exclude '*key' --exclude 'db' --exclude 'compiled'
