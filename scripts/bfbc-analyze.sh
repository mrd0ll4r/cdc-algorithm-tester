#!/bin/bash -e

source scripts/utils.sh

rm -rf "$DATA_PATH/bfbc"
mkdir "$DATA_PATH/bfbc"

find "$DATA_PATH/" -type f | parallel --jobs $(nproc) "$BIN -i {} bfbc analyze $DATA_PATH/bfbc/{/}.stats > $DATA_PATH/bfbc/{/}.csv"
