#!/bin/bash
# Cyclone Scheme
# https://github.com/justinethier/cyclone
#
# Copyright (c) 2014-2016, Justin Ethier
# All rights reserved.
#
# Copy API documentation to the Jekyll webpage (gh-pages)
#

TMP=/tmp/cyclone-docs

rm -rf $TMP
mkdir $TMP
mkdir $TMP/api
cp -r docs/api/ $TMP
cp docs/API.md $TMP
git checkout gh-pages
cp -r $TMP/api/ docs/
cp $TMP/API.md docs/

# Clean up main API page
echo "$(tail -n +2 docs/API.md)" > docs/API.md
sed -i -- 's/.md//g' docs/API.md
sed -i -- '1s;^;---\n\n;' docs/API.md
sed -i -- '1s;^;title: API\n;' docs/API.md
sed -i -- '1s;^;layout: main\n;' docs/API.md
sed -i -- '1s;^;---\n;' docs/API.md

# On-off replacements
sed -i -- 's/#-heap/#heap/g' docs/api/srfi/18.md

# Add Jekyll header and do global replacements
cd docs/api
for fn in `find . | grep md`; do
  echo "processing file: $fn"
  sed -i.old '1s;^;---\n\n;' $fn
  sed -i.old '1s;^;title: API\n;' $fn
  sed -i.old '1s;^;layout: main\n;' $fn
  sed -i.old '1s;^;---\n;' $fn
  rm -f $fn.old
  # Replace ".md"
  sed -i -- 's/.md//g' $fn
done

