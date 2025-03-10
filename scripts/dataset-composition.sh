#!/bin/bash

# Composition of text vs. binary files in the datasets (inferred by mime type).

source scripts/utils.sh

for dataset in "web" "code"; do
    echo "Results for $dataset:"
    text_total=0
    binary_total=0

    for file in $(find $DATA_PATH/$dataset/. -type f); do
        file_info=$(file --mime-type "$file")
        if [[ $file_info == *text* ]]; then
            text_total=$((text_total + $(du -b "$file" | cut -f1)))
        else
            binary_total=$((binary_total + $(du -b "$file" | cut -f1)))
        fi
    done

    echo "Text: $text_total"
    echo "Binary: $binary_total"
done