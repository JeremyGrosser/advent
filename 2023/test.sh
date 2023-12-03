#!/usr/bin/env bash

ALR="alr --no-tty --no-color"
#TIMEOUT="timeout --preserve-status --verbose 10.0s"
TIMEOUT=""
binary="${PWD}/bin/advent23"

set -e

assert() {
    puzzle=$1
    input=$2
    expectation=$3

    result=`$TIMEOUT $binary $puzzle $input`

    if [ "$result" != "$expectation" ]; then
        echo -e " \e[1m\e[31mFAIL\e[0m $binary $input"
        echo "Expected: $expectation"
        echo "Result:   $result"
        return 1
    fi
}

solve() {
    puzzle=$1
    input=$2
    result=`$timeout $binary $puzzle $input`
    echo -e " \e[1m\e[92m$result\e[0m"
}

${ALR} build

echo -n "1.1 "
assert 1.1 input/test1 142
solve 1.1 input/day1

echo -n "1.2 "
assert 1.2 input/test1_2 281
solve 1.2 input/day1

echo -n "2.1 "
assert 2.1 input/test2 8
solve 2.1 input/day2

echo -n "2.2 "
assert 2.2 input/test2 2286
solve 2.2 input/day2

echo -n "3.1 "
assert 3.1 input/test3 4361
solve 3.1 input/day2
