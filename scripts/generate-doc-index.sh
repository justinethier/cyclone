#!/bin/bash
# Cyclone Scheme
# https://github.com/justinethier/cyclone
#
# Copyright (c) 2014-2022, Justin Ethier
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

# --------------------------------------------------------------------------------
# Index with SEXP format (needed by Winds)
# The sed command bellow transforms...

#                                        ; newline
#- - -                                   ; hyphens used as sections divs
#[`abs`](api/scheme/base.md#abs)         ; Markdown link
#[`acos`](api/scheme/inexact.md#acos)    ; Markdown link

# ...into...

#((abs (scheme base))                     ; ((definition1 library-that-contains-it)
# (acos (scheme inexact)))                ;  (definition2 library-that-contains-it))

API_SEXP=api-index.scm
sed -e '/^-\|^$/d'           \
    -e 's/\[`/(/'            \
    -e 's/`\](api\// (/'     \
    -e 's/.md.*$/))/'        \
    -e 's/\// /g'            \
    -e 's/[[:space:]]\+/ /g' $API > $API_SEXP

# Add extra opening and closing parentheses
sed -e '1s/^/(/'             \
    -e '$s/$/)/'             \
    -i $API_SEXP
