#!/usr/bin/env bash

set -euo pipefail

ISA=ISA_v85A_AArch32_xml_00bet9
ISA_mini=ISA_uboot_req
cd `dirname "$BASH_SOURCE"`
cd ..

cd ./dismantle-arm-xml/data
rm -rf $ISA-all
rm -rf $ISA

git checkout $ISA
git checkout $ISA_mini

cd ../test/
git checkout ./bin
rm -rf ./bin-all
