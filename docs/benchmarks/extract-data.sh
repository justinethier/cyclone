#!/bin/bash
grep -i elapsed "$1"  | sed 's/:/ /g'
