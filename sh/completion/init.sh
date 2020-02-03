#!/bin/bash

d=$(readlink -f $(dirname "$_"))
export PATH=${PATH}:$d

. ./cmd0init.sh
