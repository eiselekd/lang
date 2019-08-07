# https://stedolan.github.io/jq/manual/#Basicfilters
# https://stedolan.github.io/jq/manual/#Invokingjq

# use nameref "-n" variable given as name
function printar() {
    local -n larray
    larray=$1
    for i in $(seq 0 $(( ${#larray[@]}-1 )) ); do echo $i: ${larray[$i]}; done
}

cat j.json | jq '.[] | .manifests |.[0] | { a : [ .deeps[].n ] } '

# handle newline
for i in $(cat j.json | jq  -r '.[] | .vendor | @base64'); do
    echo $(echo ${i} | base64 --decode)
done

# handle newline, return obj
for i in $(cat j.json | jq -r '.[] | { v : .vendor } | @base64'); do
    echo $(echo ${i} | base64 --decode | jq -r ' .v' )
done


#echo $a
#printar a
