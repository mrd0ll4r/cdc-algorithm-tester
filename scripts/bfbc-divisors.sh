#!/bin/bash

source scripts/utils.sh

echo "dataset,chunk_size,divisors"
for dataset in "${DATASETS[@]}"; do
    for chunk_size in "${TARGET_CHUNK_SIZES[@]}"; do
        divisors="$(python3 scripts/get-bfbc-divisors.py "$dataset" "$chunk_size" 0)"
        echo "$dataset,$chunk_size,$divisors"
    done
done