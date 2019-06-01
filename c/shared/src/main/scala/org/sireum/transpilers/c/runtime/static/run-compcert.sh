#!/bin/bash -e
compcert -I. -Isrc -I../shared/src/32 -o main src/stackframe.c src/misc.c types.c main.c
echo "Executing main ..."
./main
