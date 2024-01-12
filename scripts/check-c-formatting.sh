#!/bin/bash

FORMAT_CMD="indent -linux -l80 -i2 -nut"
echo $1

$FORMAT_CMD $1 -o $1.format-test

diff $1 $1.format-test > /dev/null
#ret=$?
#
#if [[ $ret -eq 0 ]]; then
#    echo "passed."
#else
#    echo "failed."
#fi
