#!/bin/bash

FORMAT_CMD="indent -linux -l80 -i2 -nut"
FILE=$1
TMP=$(mktemp)

$FORMAT_CMD $FILE -o $TMP

diff $FILE $TMP > /dev/null
#ret=$?
#
#if [[ $ret -eq 0 ]]; then
#    echo "passed."
#else
#    echo "failed."
#fi
