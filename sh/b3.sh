#!/bin/sh

fn="my name with space .and.points..txt"
IFS='.'; a=(${fn})
echo ${a[-1]}
IFS=' '

# http://tldp.org/LDP/abs/html/parameter-substitution.html
arg=$fn
afterdot=
echo before0:${arg%.*}  # remove shortest back
echo before1:${arg%%.*} # remove longest back
echo after  :${arg#*.}  # remove shortest front (return up to first .

# http://tldp.org/LDP/abs/html/parameter-substitution.html
# remove leading whitespace:
fn="  Test";
echo ":${fn##* }" # everything and one space 'fn="  Test a"' cause problems
shopt -s extglob
echo ":${fn##+( )}"


fn="abcabcTest";
echo ":${fn##[abc]}" # everything and one space

# grep -o : print match only
# grep -P : perl regex
# $(?> ...) positive lookbehind (dont include)
# $(?= ...) positive looahead  (dont include)
echo $(echo "hello this0 all" | grep -oP "(?<=(?:hello ))(?:this[0-9])")
echo $(echo "hello this0 all" | grep -oP "(?:this[0-9])(?=(?: all))")

# using BASH_REMATCH
regex="[0-9]+_([a-z]+)_[0-9a-z]*"
for f in 0_test_2 1_test_3
do
    if [[ $f =~ $regex ]]
    then
        echo "${BASH_REMATCH[1]}"
    fi
done

echo $(echo 0_test_4 | sed -E 's/([0-9]+_([a-z]+)_([0-9a-z]*))|.*/\2 \3/')
