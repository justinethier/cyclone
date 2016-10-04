#!/bin/bash

# Make sure there is a space between the header indicator and the label
sed -i -- 's/^\(#\)\([^ ]\)/# \2/g' $1

