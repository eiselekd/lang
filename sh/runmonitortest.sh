#!/bin/sh
idx=0
while true; do
    sleep 1;
    echo "p:${idx}"
    idx=$(($idx+1))
done
