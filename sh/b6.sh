function  f0 {
    return 1
}

if f0; then
    echo "f0: "
else
    echo "f0: not"
fi

a="///a//b"
#shopt -s extglob
echo ${a//\//}
