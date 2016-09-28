#!/bin/bash
# Generate a sorted list of functions/variables from the API documentation.
API=api-index.txt
TMP=/tmp/api-index.txt
cyclone scripts/generate-doc-index.scm
grep -r "^- \[" docs/api/scheme/* | ./scripts/generate-doc-index | sort > $TMP
grep -r "^- \[" docs/api/srfi/* | ./scripts/generate-doc-index >> $TMP
sort $TMP > $API
