#!/bin/bash -e

source scripts/utils.sh

dd if=/dev/urandom of="$DATA_PATH/random.bin" bs=10G count=1
