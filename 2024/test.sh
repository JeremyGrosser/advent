#!/bin/bash

ARGS=$@

check() {
    cmd="bin/advent24 $ARGS $1 $2"
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
    cmd="bin/advent24 $ARGS $1 $2"
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

check 1.1 input/test1.1 0
solve 1.1 input/day1
