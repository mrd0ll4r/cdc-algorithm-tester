#!/bin/bash

source scripts/utils.sh

for dataset in "${DATASETS[@]}"; do
    gzip -k "$DATA_PATH/$dataset"

    # Capture the byte sizes
    compressed_size=$(du -b "$DATA_PATH/$dataset.gz" | cut -f1)
    original_size=$(du -b "$DATA_PATH/$dataset" | cut -f1)

    # Perform the division
    if [ "$original_size" -ne 0 ]; then
        ratio=$((compressed_size * 100 / original_size))
        echo "Compression ratio of $dataset (as a percentage): $ratio%"
    else
        echo "Original file size is zero, cannot perform division."
    fi

    rm "$DATA_PATH/$dataset.gz"
done
