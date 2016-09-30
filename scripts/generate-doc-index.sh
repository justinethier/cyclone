#!/bin/bash
# Generate a sorted list of functions/variables from the API documentation.
API=api-index.txt
TMP=/tmp/api-index.txt
cyclone scripts/convert-doc-index.scm
cyclone scripts/alphabetize.scm
#grep -r "^- \[" docs/api/primitives.md | ./scripts/convert-doc-index > $TMP
grep -r "^- \[" docs/api/scheme/* | ./scripts/convert-doc-index > $TMP
grep -r "^- \[" docs/api/scheme/cyclone/* | ./scripts/convert-doc-index > $TMP
grep -r "^- \[" docs/api/srfi/* | ./scripts/convert-doc-index >> $TMP
sort $TMP | ./scripts/alphabetize > $API
