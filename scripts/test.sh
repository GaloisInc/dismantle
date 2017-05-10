#!/usr/bin/env bash

ARCH=$1

case $ARCH in
    tablegen)
        stack build dismantle-tablegen --test
        ;;
    ppc)
        stack build dismantle-ppc --test --test-arguments='--hide-successes'
        ;;
    *)
        echo "Unrecognized TEST_ARCH=${ARCH}"
        ;;
esac
