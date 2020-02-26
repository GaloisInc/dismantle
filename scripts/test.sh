#!/usr/bin/env bash

set -euo pipefail

ARCH=$1

export STACK_YAML=stack-ghc-8.0.2.yaml
export PATH=/opt/cabal/bin:$PATH

if [[ -f "./cabal.project.newbuild" && ! -f "./cabal.project" ]]; then
    cp ./cabal.project.newbuild ./cabal.project
fi
cabal v2-update

case $ARCH in
    tablegen)
        cabal v2-test dismantle-tablegen
        ;;
    ppc)
        cabal v2-test dismantle-ppc
        ;;
    arm)
        cabal v2-test dismantle-arm
        ;;
   arm-xml-mini)
        ./scripts/minify-asl.sh
        cabal v2-test dismantle-arm-xml || ./scripts/deminify-asl.sh
        ./scripts/deminify-asl.sh
        ;;
    arm-xml)
        cabal v2-test dismantle-arm-xml
        ;;
    thumb)
        cabal v2-test dismantle-thumb
        ;;
    aarch64)
        cabal v2-test dismantle-aarch64
        ;;
    *)
        echo "Unrecognized TEST_ARCH=${ARCH}"
        ;;
esac
