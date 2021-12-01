#!/bin/sh
PATH=$PATH:$PWD/bin
set -e -x
alr --no-tty -q build
d1 < inputs/d1-test
d1 < inputs/d1
