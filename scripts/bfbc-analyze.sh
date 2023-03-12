#!/bin/bash

BIN=./target/release/cdc-algorithm-tester
DATA_PATH=data

rm -rf $DATA_PATH/bfbc
mkdir $DATA_PATH/bfbc

for file in $DATA_PATH/*; do
  $($BIN -i $file bfbc analyze $DATA_PATH/bfbc/$(basename $file).stats)
done