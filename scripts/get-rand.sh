#!/bin/bash -e

source scripts/utils.sh

dd if=/dev/urandom of="$DATA_PATH/random.bin" bs=1G count=10
dd if=/dev/urandom of="$DATA_PATH/random_small.bin" bs=1G count=1
