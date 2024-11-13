#!/bin/bash

check() {
    cmd="bin/advent24 $1 $2"
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

alr build

check 1.1 input/day1 42
