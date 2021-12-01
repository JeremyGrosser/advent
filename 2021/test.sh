#!/bin/sh
PATH=$PATH:$PWD/bin
set -e -x
alr --no-tty build

d1_1 < inputs/d1-test
d1_1 < inputs/d1
d1_2 < inputs/d1-test
d1_2 < inputs/d1
