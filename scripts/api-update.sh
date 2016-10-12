#!/bin/bash
# Cyclone Scheme
# https://github.com/justinethier/cyclone
#
# Copyright (c) 2014-2016, Justin Ethier
# All rights reserved.
#
# Purpose
# Use this script to copy API documentation changes to the Jekyll webpage (gh-pages).
#
# Usage
# Run this script from the main directory of the repo:
#     $ ./scripts/api-update.sh
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

# Do any one-off replacements
cd ../..
sed -i -- 's/primitives#)/primitives#section)/g' docs/API.md
sed -i -- 's/primitives#-1)/primitives#section-1)/g' docs/API.md
sed -i -- 's/primitives#-2)/primitives#section-2)/g' docs/API.md
sed -i -- 's/primitives#-3)/primitives#section-3)/g' docs/API.md
sed -i -- 's/primitives#-4)/primitives#section-4)/g' docs/API.md
sed -i -- 's/primitives#-5)/primitives#section-5)/g' docs/API.md
sed -i -- 's/primitives#-6)/primitives#section-6)/g' docs/API.md
sed -i -- 's/primitives#-7)/primitives#section-7)/g' docs/API.md
sed -i -- 's/primitives#-8)/primitives#section-8)/g' docs/API.md
sed -i -- 's/(#)/(#section)/g' docs/api/primitives.md
sed -i -- 's/(#-1)/(#section-1)/g' docs/api/primitives.md
sed -i -- 's/(#-2)/(#section-2)/g' docs/api/primitives.md
sed -i -- 's/(#-3)/(#section-3)/g' docs/api/primitives.md
sed -i -- 's/(#-4)/(#section-4)/g' docs/api/primitives.md
sed -i -- 's/(#-5)/(#section-5)/g' docs/api/primitives.md
sed -i -- 's/(#-6)/(#section-6)/g' docs/api/primitives.md
sed -i -- 's/(#-7)/(#section-7)/g' docs/api/primitives.md
sed -i -- 's/(#-8)/(#section-8)/g' docs/api/primitives.md
sed -i -- 's/(#)/(#section)/g' docs/api/srfi/128.md
sed -i -- 's/(#-1)/(#section-1)/g' docs/api/srfi/128.md
sed -i -- 's/(#-2)/(#section-2)/g' docs/api/srfi/128.md
sed -i -- 's/(#-3)/(#section-3)/g' docs/api/srfi/128.md
sed -i -- 's/(#-4)/(#section-4)/g' docs/api/srfi/128.md
sed -i -- 's/#-heap/#heap/g' docs/API.md
sed -i -- 's/#-heap/#heap/g' docs/api/srfi/18.md

