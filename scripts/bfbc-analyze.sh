#!/bin/bash

BIN=./target/release/cdc-algorithm-tester
DATA_PATH=data

rm -rf $DATA_PATH/bfbc
mkdir $DATA_PATH/bfbc

find $DATA_PATH/ -type f | parallel --jobs $(nproc) "$BIN -i {} bfbc analyze $DATA_PATH/bfbc/{/.}.stats"
