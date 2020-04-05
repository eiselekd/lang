
function jq_url_of_remote () {
    local u
    local -n l_url
    l_url=$1
    l_url=()
    for u in $(cat j1.json | jq -r ".[] | select(.id==\"${2}\") | .remotes | .[] | select(.name==\"${3}\") | .urls | .[] "); do
	echo "0: $u"
	l_url+=("$u")
    done
}

local -a jq_urls
jq_url_of_remote jq_urls q q0

echo "_0: ${jq_urls[0]}"
for u in ${jq_urls[@]:1:$((${#jq_urls[@]}-1))}; do
    echo "more: ${u}"
done
