#!/bin/bash -e

source scripts/utils.sh

dd if=/dev/zero of="$DATA_PATH/zero.bin" bs=2G count=1
