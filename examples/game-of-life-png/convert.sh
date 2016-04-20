#!/bin/bash
rm -rf tmp
mkdir tmp
for f in `find . -name "*.png"`
do
  convert -resize 100x $f tmp/$f.resize.png
done
cd tmp
convert -delay 10 -loop 0 *.png animated.gif

