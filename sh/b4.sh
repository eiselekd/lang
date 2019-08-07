#!/bin/sh

function a0 () {
    echo "a0"
    return 0
}

function a1 () {
    echo "a1"
    return 1
}

function a2 () {
    echo "a2"
    return 0
}

a0 && a1 && a2
