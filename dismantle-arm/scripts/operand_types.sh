#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

TGEN=$HERE/../data/ARM.tgen

grep OperandList $TGEN \
    | awk -F'= ' '{ print $2 }' \
    | sed 's/ins //g' \
    | sed 's/outs //g' \
    | sed 's/(//' \
    | sed 's/);//' \
    | grep -v ins \
    | grep -v outs \
    | awk -F', ' '{ for (i = 1; i <= NF; i++) { print $i } }' \
    | grep -v variable_ops \
    | awk -F':' '(NF == 2) { print "concrete:" $1 } (NF > 2) { print "indirect:" $0 }' \
    | sort \
    | uniq
