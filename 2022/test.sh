#!/usr/bin/env bash

ALR="alr --no-tty --no-color -q"

set -e

assert() {
    crate=$1
    binary=$crate/bin/$crate
    input=$2
    expectation=$3

    pushd $crate
    ${ALR} --no-tty build
    popd

    result=`$binary < $input`

    if [ "$result" != "$expectation" ]; then
        echo -e " \e[1m\e[31mFAIL\e[0m $binary < $input"
        echo "Expected: $expectation"
        echo "Result:   $result"
        return 1
    fi
}

solve() {
    crate=$1
    binary=$crate/bin/$crate
    input=$2
    result=`$binary < $input`
    echo -e " \e[1m\e[92m$result\e[0m"
}

echo -n "1.1 "
assert day1_1 input/day1_test 24000
solve day1_1 input/day1
assert day1_2 input/day1_test 45000
solve day1_2 input/day1
