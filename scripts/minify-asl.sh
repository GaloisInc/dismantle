#!/usr/bin/env bash

set -euo pipefail

ISA=ISA_v85A_AArch32_xml_00bet9

cd ./dismantle-arm-xml/data
rm -rf alldata
mv $ISA alldata
mkdir $ISA
mv ./alldata/mov* ./$ISA
mv ./alldata/adc* ./$ISA
mv ./alldata/a32_encindex.xml ./$ISA
mv ./alldata/t32_encindex.xml ./$ISA
