#!/bin/bash
# https://stedolan.github.io/jq/manual/#Basicfilters
# https://stedolan.github.io/jq/manual/#Invokingjq

declare -a a
a=(4 5 6)

# use nameref "-n" variable given as name
function printar() {
    local -n larray
    larray=$1
    for i in $(seq 0 $(( ${#larray[@]}-1 )) ); do echo $i: ${larray[$i]}; done
}

function p0() {
    local -a a
    a=(1 2 3)
    printar a
}

p0

printar a
#echo $a
#printar a
