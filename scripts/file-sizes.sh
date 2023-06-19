#!/bin/bash

# This script requires an env variable DATASET set and will print a comma separated list of file sizes within this dataset.

source scripts/utils.sh
tar -tvf "$DATA_PATH/$DATASET.tar" | awk '$1 ~ /^-/{print $3}' | paste -sd, -
