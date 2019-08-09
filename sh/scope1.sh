#!/bin/bash
# https://stedolan.github.io/jq/manual/#Basicfilters
# https://stedolan.github.io/jq/manual/#Invokingjq

declare -a ar
ar=(10 11 12)

# use nameref "-n" variable given as name
function printar() {
    local -n larray
    larray=$1
    for i in $(seq 0 $(( ${#larray[@]}-1 )) ); do echo $i: ${larray[$i]}; done
}

function proc () {
    . ./scope1_load.sh
    printar ar
}

proc
echo "After proc:"
printar ar
