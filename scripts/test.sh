#!/usr/bin/env bash

set -euo pipefail

ARCH=$1

export STACK_YAML=stack-ghc-8.0.2.yaml

case $ARCH in
    tablegen)
        stack build dismantle-tablegen --system-ghc --test
        ;;
    ppc)
        # Travis pretends to offer a fully functional TTY, but it slurps up all
        # of the output for logging.  With --hide-successes, tasty uses TTY
        # magic to rewrite the same line repeatedly to prevent scrolling.  These
        # two things seem to interact poorly, and travis logs every line that
        # eventually gets rewritten; once it hits 4MB, it kills the job.
        #
        # Pipe the output of the test suite to cat so that tasty doesn't think
        # it has a TTY and really doesn't produce any success output.  This has
        # the nice side effect of being much faster.
        stack build dismantle-ppc --system-ghc --test --test-arguments='--hide-successes' | cat -
        ;;
    arm)
        stack build dismantle-arm --system-ghc --test --test-arguments='--hide-successes' | cat -
        ;;
    thumb)
        stack build dismantle-thumb --system-ghc --test --test-arguments='--hide-successes' | cat -
        ;;
    aarch64)
        stack build dismantle-aarch64 --system-ghc --test --test-arguments='--hide-successes' | cat -
        ;;
    *)
        echo "Unrecognized TEST_ARCH=${ARCH}"
        ;;
esac
