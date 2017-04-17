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

# Expected line structure:
# dag OutOperandList = (outs QPR:$Vd);
# dag InOperandList = (ins QPR:$Vm);

# Only process lines that mention operand lists. This rules out lines like
# dag InOperandList = !con(AExtI:iops, (ins pred:$p));
grep OperandList $TGEN | {
    # Extract the right hand side of each operand list assignment.
    awk -F'= ' '{ print $2 }'
} | {
    # Permit only lines that begin with ins/outs (remove lines like
    # "!con(...")
    grep -E '^\((ins|outs)'
} | {
    # Remove ins keywords
    sed 's/ins //g'
} | {
    # Remove outs keywords
    sed 's/outs //g'
} | {
    # Remove parens
    sed 's/^(//'
} | {
    sed 's/);$//'
} | {
    # At this point, lines that initially read "... = (ins);" will just
    # say "ins", so we need to remove those.
    grep -vx ins
} | {
    # At this point, lines that initially read "... = (outs);" will just
    # say "outs", so we need to remove those.
    grep -vx outs
} | {
    # For each argument in "a:b, c:d, ...", print one argument per line.
    awk -F', ' '{ for (i = 1; i <= NF; i++) { print $i } }'
} | {
    # Remove variable_ops arguments.
    grep -v variable_ops
} | {
    # For each line of the form "a:b", prepend a concrete prefix since
    # we think those refer to concrete operand types. For each line of
    # the form "a:b:c", prepend an indirect prefix since those typically
    # refer to types of class attributes. If we encounter one with more
    # components than expected, use the prefix "bad:".
    awk -F':' '(NF == 2) { print "concrete:" $1 } \
               (NF == 3) { print "indirect:" $1 ":" $2 } \
               (NF > 3)  { print "bad:" $0 }'
} | {
    # Sort ...
    sort
} | {
    # ... and eliminate duplicates.
    uniq
}
