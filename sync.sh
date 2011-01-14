#!/bin/sh
rsync -av --progress . grad-admissions:local/m8b/ --exclude 'admissions*' --exclude '*pem' --exclude '*key' --exclude 'db' --exclude 'compiled'