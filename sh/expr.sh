#!/bin/bash
a="[test] 129"
if ! [[ $a =~ "[test] -f  18" ]]; then
    echo "not Found : '$a'"
    b=$(expr "$a" : '\[[a-z]*\].*\([0-9][0-9][0-9].*\)')
    echo "[test] -f $b"
fi

a="[test] testall -f out.txt"
if ! [[ $a =~ "\[([a-z]+)\].*\-d ([a-z\.]+)" ]]; then
    echo "Found : '$a'"
fi
