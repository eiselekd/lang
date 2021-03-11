f=$(echo -en "a\tb")
regex="(a[[:space:]]+b)"
if [[ $f =~ $regex ]]
then
    name="${BASH_REMATCH[1]}"
    echo "${name}"    # concatenate strings
else
    echo "nomatch:$f"
fi
