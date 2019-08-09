#!/bin/sh

declare -a ar
ar=(1 2 3 4 5)
echo "${ar[@]:1:$((${#ar[@]}-1))}"
ar=(1)
echo "${ar[@]:1:$((${#ar[@]}-1))}"
echo "${@:0:1}"
