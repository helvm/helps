#!/bin/bash

TIMEOUT=60
# Very meta. We self-compile three times because if a change was made
# the compiler's output in classy.hs, we need to compile two more
# times to propagate that change.
stage1() {
    echo 'n' | timeout $TIMEOUT ./build/blynn .output/actual/mini-haskell/classy "$1" build/classy2 ||
        (echo 'Stage 1 fail' && exit 1)
}

stage2() {
    echo 'n' | timeout $TIMEOUT ./build/blynn build/classy2 "$1" build/classy3 ||
        (echo 'Stage 2 fail' && exit 1)
}

stage3() {
    echo 'n' | timeout $TIMEOUT ./build/blynn build/classy3 "$1" build/classy4 ||
        (echo 'Stage 3 fail' && exit 1)
}

check_compile() {
    stage1 "$1" &&
    stage2 "$1" &&
    stage3 "$1" &&
    printf '\n' &&
    diff -qs build/classy3 build/classy4
} 
if [[ $1 == "" ]]
then
    echo "./check_compile.sh <compiler source>"
    exit
fi
check_compile "$1"
