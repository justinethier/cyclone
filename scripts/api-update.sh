#!/bin/bash

rm -rf /tmp/api
cp -r docs/api/ /tmp/api
git checkout gh-pages
cp -r /tmp/api/ docs/

# find . | grep md
for fn in `find . | grep md`; do
  echo "processing file: $fn"
  sed -i.old '1s;^;---\n;' $fn
  sed -i.old '1s;^;title: API\n;' $fn
  sed -i.old '1s;^;layout: main\n;' $fn
  sed -i.old '1s;^;---\n;' $fn
  rm -f $fn.old
done

