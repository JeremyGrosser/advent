#!/bin/sh
PATH=$PATH:$PWD/bin
set -e
alr --no-tty build

assert() {
    binary=$1
    input=$2
    expectation=$3
    result=`$binary < $input`
    if [ $result != $expectation ]; then
        echo -e " \e[31mFAIL\e[0m $binary < $input"
        echo "Expected: $expectation"
        echo "Result:   $result"
        return 1
    fi
}

solve() {
    binary=$1
    input=$2
    result=`$binary < $input`
    echo -e " \e[1m\e[92m$result\e[0m"
}

echo -n "1.1"
assert d1_1 inputs/d1-test 7
solve d1_1 inputs/d1

echo -n "1.2"
assert d1_2 inputs/d1-test 5
solve d1_2 inputs/d1
