#!/bin/zsh
#. ./completion_init.sh

d=$(readlink -f $(dirname "$_"))

rm -rf ~/.zcompdump*
fpath=($d $fpath)
export PATH=${PATH}:$d

compinit
