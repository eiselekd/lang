local -A donges

IFS=$'\n'
hciline="^hci([0-9]):"
bdadd="BD Address: ([0-9A-F:]+)"
for i in $(hciconfig); do
    if [[ $i =~ $hciline ]]
    then
	n="${BASH_REMATCH[1]}"
	echo $n;
    elif [[ $i =~ $bdadd ]]
    then
	n="${BASH_REMATCH[1]}"
	echo $n;
    fi
done
