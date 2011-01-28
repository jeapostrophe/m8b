#!/bin/sh
rsync -av --progress . grad-admissions$1:local/m8b/ --exclude 'admissions*' --exclude '*pem' --exclude '*key' --exclude 'db' --exclude 'compiled'