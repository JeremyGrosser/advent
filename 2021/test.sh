#!/bin/bash
PATH=$PATH:$PWD/bin
set -e
alr --no-tty build

assert() {
    binary=$1
    input=$2
    expectation=$3
    result=`$binary < $input`
    if [ $result != $expectation ]; then
        echo -e " \e[1m\e[31mFAIL\e[0m $binary < $input"
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

echo -n "2.1"
assert d2_1 inputs/d2-test 150
solve d2_1 inputs/d2

echo -n "2.2"
assert d2_2 inputs/d2-test 900
solve d2_2 inputs/d2

echo -n "3.1"
assert d3_1 inputs/d3-test 198
solve d3_1 inputs/d3

echo -n "3.2"
assert d3_2 inputs/d3-test 230
solve d3_2 inputs/d3

echo -n "4.1"
assert d4_1 inputs/d4-test 4512
solve d4_1 inputs/d4

echo -n "4.2"
#assert d4_2 inputs/d4-test 1924
solve d4_2 inputs/d4

echo -n "5.1"
assert d5_1 inputs/d5-test 5
solve d5_1 inputs/d5

echo -n "5.2"
assert d5_2 inputs/d5-test 12
solve d5_2 inputs/d5

echo -n "6.1"
assert d6_1 inputs/d6-test 5934
solve d6_1 inputs/d6

echo -n "6.2"
assert d6_2 inputs/d6-test 26984457539
solve d6_2 inputs/d6

echo -n "7.1"
assert d7_1 inputs/d7-test 37
solve d7_1 inputs/d7

echo -n "7.2"
assert d7_2 inputs/d7-test 168
solve d7_2 inputs/d7
