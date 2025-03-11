#!/bin/bash -e

source scripts/utils.sh

mkdir -p "$DATA_PATH/random" && cd "$DATA_PATH/random"

dd if=/dev/urandom of="random.bin" bs=1G count=10
# dd if=/dev/urandom of="$DATA_PATH/random_small.bin" bs=1G count=1
