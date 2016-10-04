#!/bin/bash

TMP=/tmp/cyclone-docs

rm -rf $TMP
mkdir $TMP
mkdir $TMP/api
cp -r docs/api/ $TMP/api
cp docs/API.md $TMP/
git checkout gh-pages
cp -r $TMP/api/ docs/
cp $TMP/API.md docs/

echo "$(tail -n +2 docs/API.md)" > docs/API.md
sed -i -- 's/.md//g' docs/API.md
sed -i -- '1s;^;---\n\n;' docs/API.md
sed -i -- '1s;^;title: API\n;' docs/API.md
sed -i -- '1s;^;layout: main\n;' docs/API.md
sed -i -- '1s;^;---\n;' docs/API.md

# find . | grep md
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

