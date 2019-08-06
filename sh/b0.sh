#!/bin/sh
# process output into array
declare -a array
i=0
array[i++]=0
array[i++]=1
array[i++]=2
array[i++]=3
array[i++]=4
array+=(5)
array[${#array[@]}]=6

# iterate over array

# use nameref "-n" variable given as name
function printar() {
    local -n larray
    larray=$1
    for i in $(seq 0 $(( ${#larray[@]}-1 )) ); do echo $i: ${larray[$i]}; done
}

echo ;printar array

unset array[$((${#array[@]}-1))]
echo ;for i in $(seq 0 $(( ${#array[@]}-1 )) ); do echo $i: ${array[$i]}; done

array=("${array[@]:0:${#array[@]}-1}")
echo ;for i in $(seq 0 $(( ${#array[@]}-1 )) ); do echo $i: ${array[$i]}; done

unset array[-1] # bash 4.3
echo ;for i in $(seq 0 $(( ${#array[@]}-1 )) ); do echo $i: ${array[$i]}; done
echo ${array[-1]}

# iterate over hash
declare -A h
h[bees]=1
h[butterfly]=2

# iterate over keys
for i in ${!h[@]}; do
    echo $i: ${h[$i]}
done

unset h[butterfly]

for i in ${!h[@]}; do
    echo $i: ${h[$i]}
done

# break line according to regex
