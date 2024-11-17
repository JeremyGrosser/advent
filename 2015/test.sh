#!/bin/bash

check() {
    cmd="bin/advent15 $1 $2"
    echo "$cmd"

    result="$($cmd)"
    ret=$?

    if [ ! $ret -eq 0 ]; then
        echo ""
        echo "$1 ERROR $ret"
        exit 1
    fi

    if [ $result -eq $3 ]; then
        echo -e "$1 PASS \e[1m\e[92m$result\e[0m";
    else
        echo -e "$1 FAIL \e[1m\e[31m$result\e[0m /= $3"
        exit 1
    fi
}

solve() {
    cmd="bin/advent15 $1 $2"
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

alr build

check 1.1 input/test1.1 -3
solve 1.1 input/day1
check 1.2 input/test1.2 5
solve 1.2 input/day1

check 2.1 input/test2.1 101
solve 2.1 input/day2
