#!/bin/bash -e

source scripts/utils.sh

dd if=/dev/zero of="$DATA_PATH/zero.bin" bs=10G count=1
