#!/bin/bash
# Cyclone Scheme
# https://github.com/justinethier/cyclone
#
# Copyright (c) 2014-2016, Justin Ethier
# All rights reserved.
#
# Generate a sorted list of functions/variables from the API documentation.
#
API=api-index.txt
TMP=/tmp/api-index.txt
cyclone scripts/convert-doc-index.scm
cyclone scripts/alphabetize.scm
grep "^- \[" docs/api/* | ./scripts/convert-doc-index > $TMP
grep -r "^- \[" docs/api/cyclone/* | ./scripts/convert-doc-index >> $TMP
grep -r "^- \[" docs/api/scheme/* | ./scripts/convert-doc-index >> $TMP
grep -r "^- \[" docs/api/srfi/* | ./scripts/convert-doc-index >> $TMP
grep -r "^\[" docs/api/srfi/* | ./scripts/convert-doc-index >> $TMP
grep -r "^\[" docs/api/cyclone/* | ./scripts/convert-doc-index >> $TMP
sort $TMP | ./scripts/alphabetize > $API
