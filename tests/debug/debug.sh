#!/bin/bash

while [ 1 ]
do
  gdb --eval-command=run --eval-command=quit  --args cyclone cyclone.scm
done
