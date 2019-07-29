#!/bin/sh
#
# A simple script to automate copying changed files to cyclone-bootstrap.
# Sometimes this is easier than rebuilding everything from this repo.
#
DEST=../cyclone-bootstrap
for var in "$@"
do
  echo "cp $var $DEST/$var"
  cp $var $DEST/$var
  case "$var" in
      *.sld) echo "cp `dirname $var`/`basename $var .sld`.c $DEST/`dirname $var`/`basename $var .sld`.c" ; cp `dirname $var`/`basename $var .sld`.c $DEST/`dirname $var`/`basename $var .sld`.c ;;
  esac
done
