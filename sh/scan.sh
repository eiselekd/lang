declare -A mac
declare -a dongles

IFS=$'\n'
hciline="^(hci[0-9]+):"
bdadd="BD Address: ([0-9A-F:]+)"
c=""
for i in $(hciconfig); do
    if [[ $i =~ $hciline ]]
    then
	c="${BASH_REMATCH[1]}"
	dongels+=$c
    elif [[ $i =~ $bdadd ]]
    then
	mac[$c]="${BASH_REMATCH[1]}"
    fi
done

for m in ${!dongels[@]}; do
    echo $m ${dongels[$m]}
done

for m in ${!mac[@]}; do
    echo $m ${mac[$m]}
done
