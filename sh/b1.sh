#!/bin/sh
declare -a array

# use nameref "-n" variable given as name
function printar() {
    local -n larray
    larray=$1
    for i in $(seq 0 $(( ${#larray[@]}-1 )) ); do echo $i: ${larray[$i]}; done
}

# break line according to regex
IFS=$'\n' # this unescapes
array=($(cat d.txt))

printar array

# break line according to regex
IFS=$'\n|[a]' # this unescapes
array=($(cat d.txt))
printar array

var1="a+b+c"
var2="d-e-f"
var3="g,h,i"

#http://www.tldp.org/LDP/abs/html/abs-guide.html#IFSH
IFS=+
# The plus sign will be interpreted as a separator.
echo $var1     # a b c
echo $var2     # d-e-f
echo $var3     # g,h,i

output_args_one_per_line()
{
  for arg
  do
    echo "[$arg]"
  done #  ^    ^   Embed within brackets, for your viewing pleasure.
}

echo; echo "IFS=\" \""
echo "-------"

IFS=" "
var=" a  b c   "
#    ^ ^^   ^^^
output_args_one_per_line $var  # output_args_one_per_line `echo " a  b c   "`
# [a]
# [b]
# [c]


echo; echo "IFS=:"
echo "-----"

IFS=:
var=":a::b:c:::"               # Same pattern as above,
#    ^ ^^   ^^^                #+ but substituting ":" for " "  ...
output_args_one_per_line $var
# []
# [a]
# []
# [b]
# [c]
# []
# []

# Note "empty" brackets.
# The same thing happens with the "FS" field separator in awk.



output_args_one_per_line 1 2 3
