
regex="^remote\.[0-9a-z\-]+(__[0-9]+)?\.url=(.+)"

if [[ "remote.test__0.url=a" =~ $regex ]]; then
    echo "${BASH_REMATCH[2]}"
fi

if [[ "remote.test.url=b" =~ $regex ]]; then
    echo "${BASH_REMATCH[2]}"
fi


regex="^remote\.(([0-9a-z\-]+)(__[0-9]+)?)\.url=(.+)"
if [[ "remote.test__0.url=a" =~ $regex ]]; then
    echo "${BASH_REMATCH[2]}"
fi
if [[ "remote.test.url=b" =~ $regex ]]; then
    echo "${BASH_REMATCH[2]}"
fi

regex="^test(__[0-9]+)?\$"
if [[ "atest" =~ $regex ]]; then
    echo "match 0"
fi
if [[ "test" =~ $regex ]]; then
    echo "match 1"
fi
if [[ "test0" =~ $regex ]]; then
    echo "match 1.1"
fi
if [[ "test__0" =~ $regex ]]; then
    echo "match 2"
fi
