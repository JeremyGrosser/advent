#!/usr/bin/env bash

ALR="alr --no-tty --no-color"
binary="${PWD}/bin/advent2022"

set -e

assert() {
    puzzle=$1
    input=$2
    expectation=$3

    result=`$binary $puzzle < $input`

    if [ "$result" != "$expectation" ]; then
        echo -e " \e[1m\e[31mFAIL\e[0m $binary < $input"
        echo "Expected: $expectation"
        echo "Result:   $result"
        return 1
    fi
}

solve() {
    puzzle=$1
    input=$2
    result=`$binary $puzzle < $input`
    echo -e " \e[1m\e[92m$result\e[0m"
}

${ALR} build

echo -n "1.1 "
assert 1.1 input/day1_test 24000
solve 1.1 input/day1

echo -n "1.2 "
assert 1.2 input/day1_test 45000
solve 1.2 input/day1

echo -n "2.1 "
assert 2.1 input/day2_test 15
solve 2.1 input/day2
