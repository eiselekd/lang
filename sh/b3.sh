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
fn="  Test a";
echo ":${fn##+ }" # everything and one space

fn="abcabcTest";
echo ":${fn##[abc]}" # everything and one space
