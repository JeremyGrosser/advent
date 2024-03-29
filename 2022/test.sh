#!/usr/bin/env bash

ALR="alr --no-tty --no-color"
#TIMEOUT="timeout --preserve-status --verbose 10.0s"
TIMEOUT=""
binary="${PWD}/bin/advent2022"

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
assert 1.1 input/day1_test 24000
solve 1.1 input/day1

echo -n "1.2 "
assert 1.2 input/day1_test 45000
solve 1.2 input/day1

echo -n "2.1 "
assert 2.1 input/day2_test 15
solve 2.1 input/day2

echo -n "2.2 "
assert 2.2 input/day2_test 12
solve 2.2 input/day2

echo -n "3.1 "
assert 3.1 input/day3_test 157
solve 3.1 input/day3

echo -n "3.2 "
assert 3.2 input/day3_test 70
solve 3.2 input/day3

echo -n "4.1 "
assert 4.1 input/day4_test 2
solve 4.1 input/day4

echo -n "4.2 "
assert 4.2 input/day4_test 4
solve 4.2 input/day4

echo -n "5.1 "
assert 5.1 input/day5_test CMZ
solve 5.1 input/day5

echo -n "5.2 "
assert 5.2 input/day5_test MCD
solve 5.2 input/day5

echo -n "6.1 "
assert 6.1 input/day6_test 5
solve 6.1 input/day6

echo -n "6.2 "
assert 6.2 input/day6_test 23
solve 6.2 input/day6

echo -n "7.1 "
assert 7.1 input/day7_test 95437
solve 7.1 input/day7

echo -n "7.2 "
assert 7.2 input/day7_test 24933642
solve 7.2 input/day7

echo -n "8.1 "
assert 8.1 input/day8_test 21
solve 8.1 input/day8

echo -n "8.2 "
assert 8.2 input/day8_test 8
solve 8.2 input/day8

echo -n "9.1 "
assert 9.1 input/day9_test 13
solve 9.1 input/day9

echo -n "9.2 "
assert 9.2 input/day9_test 1
assert 9.2 input/day9_test2 36
solve 9.2 input/day9

echo -n "10.1"
assert 10.1 input/day10_test 13140
solve 10.1 input/day10

#echo -n "10.2"
#assert 10.2 input/day10_test 13140
#solve 10.2 input/day10

echo -n "11.1"
assert 11.1 input/day11_test 10605
solve 11.1 input/day11

echo -n "11.2"
assert 11.2 input/day11_test 2713310158
solve 11.2 input/day11

echo -n "12.1"
assert 12.1 input/day12_test 31
solve 12.1 input/day12
