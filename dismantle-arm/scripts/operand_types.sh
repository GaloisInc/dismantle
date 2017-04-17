#!/usr/bin/env bash

# This script processes the ARM tgen data to extract all mentioned
# input and output operand types. ARM operand types fall into
# two categories: concrete (some_type:name) and indirect
# (some_class:some_attribute:name). This script classifies each and
# marks each with a prefix of either "concrete:" or "indirect:".
# Duplicates are eliminated so the output is the unique list of operand
# types.

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
    | awk -F':' '(NF == 2) { print "concrete:" $1 } (NF == 3) { print "indirect:" $1 ":" $2 }' \
    | sort \
    | uniq
