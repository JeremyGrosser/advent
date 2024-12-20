#!/bin/bash

ARGS=$@

check() {
    cmd="bin/advent15 $ARGS $1 $2"
    echo "$cmd"

    result="$($cmd)"
    ret=$?

    if [ ! $ret -eq 0 ]; then
        echo ""
        echo "$1 ERROR $ret"
        exit 1
    fi

    if [ -z "$result" ]; then
        echo -e "$1 FAIL \e[1m\e[31m(null)\e[0m /= $3"
        exit 1
    fi

    if [ ! $result -eq $3 ]; then
        echo -e "$1 FAIL \e[1m\e[31m$result\e[0m /= $3"
        exit 1
    #else
    #    echo -e "$1 PASS \e[1m\e[92m$result\e[0m";
    fi
}

solve() {
    cmd="bin/advent15 $ARGS $1 $2"
    echo "$cmd"

    result="$($cmd)"
    ret=$?

    if [ ! $ret -eq 0 ]; then
        echo ""
        echo "$1 ERROR $ret"
        exit 1
    fi

    echo -e "$1 \e[1m\e[92m$result\e[0m";
}

alr build --development
ret=$?
if [ ! $ret -eq 0 ]; then
    exit $ret
fi

check 1.1 input/test1.1 -3
solve 1.1 input/day1
check 1.2 input/test1.2 5
solve 1.2 input/day1

check 2.1 input/test2.1 101
solve 2.1 input/day2
check 2.2 input/test2.2 48
solve 2.2 input/day2

check 3.1 input/test3.1 4
check 3.1 input/test3.1-2 2
check 3.1 input/test3.1-3 2
solve 3.1 input/day3

check 3.2 input/test3.1-2 11
solve 3.2 input/day3

check 4.1 input/test4.1 609043
solve 4.1 input/day4
#solve 4.2 input/day4

check 5.1 input/test5.1 2
solve 5.1 input/day5
check 5.2 input/test5.2 6
solve 5.2 input/day5

check 6.1 input/test6.1 998996
solve 6.1 input/day6
check 6.2 input/test6.2 2000001
solve 6.2 input/day6
