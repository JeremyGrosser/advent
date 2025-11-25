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

    if [ ! "$result" = "$3" ]; then
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

#check 1.1 input/test1.1 11
#solve 1.1 input/day1
#check 1.2 input/test1.1 31
#solve 1.2 input/day1

#check 2.1 input/test2.1 2
#solve 2.1 input/day2
#check 2.2 input/test2.1 4
#solve 2.2 input/day2

#check 3.1 input/test3.1 161
#solve 3.1 input/day3
#check 3.2 input/test3.2 48
#solve 3.2 input/day3

#check 4.1 input/test4.1 18
#solve 4.1 input/day4
#check 4.2 input/test4.1 9
#solve 4.2 input/day4

#check 5.1 input/test5.1 143
#solve 5.1 input/day5
#check 5.2 input/test5.1 123
#solve 5.2 input/day5

#check 6.1 input/test6.1 41
#solve 6.1 input/day6
#check 6.2 input/test6.1 6
#solve 6.2 input/day6

#check 7.1 input/test7.1 3749
#solve 7.1 input/day7
#check 7.2 input/test7.1 11387
#solve 7.2 input/day7

#check 8.1 input/test8.1.1 2
#check 8.1 input/test8.1.2 4
#check 8.1 input/test8.1 14
#solve 8.1 input/day8
#check 8.2 input/test8.1 34
#solve 8.2 input/day8

#check 9.1 input/test9.1 1928
#solve 9.1 input/day9
#check 9.2 input/test9.1 2858
#solve 9.2 input/day9

#check 10.1 input/test10.2 2
#check 10.1 input/test10 36
#solve 10.1 input/day10
#check 10.2 input/test10.3 3
#solve 10.2 input/day10

#check 11.1 input/test11.1 55312
#solve 11.1 input/day11
#solve 11.2 input/day11

#check 14.1 input/test14 12
#solve 14.1 input/day14
#solve 14.2 input/day14

#check 15.1 input/test15.1 10092
#check 15.1 input/test15.2 2028
#solve 15.1 input/day15
#check 15.2 input/test15.3 105
#check 15.2 input/test15.1 9021
#solve 15.2 input/day15

#check 17.1 input/test17 4,6,3,5,6,3,5,2,1,0
#solve 17.1 input/day17
#check 17.2 input/test17.3 117440
#solve 17.2 input/day17

check 25.1 input/test25 3
solve 25.1 input/day25
