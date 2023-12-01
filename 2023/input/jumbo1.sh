#!/bin/bash
rm -f day1.jumbo
for((i=0;i<5000;i++)); do
    cat day1 >>day1.jumbo
done
