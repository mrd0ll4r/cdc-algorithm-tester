#!/bin/bash

echo -e "seq_length\tskip_trigger\tskip_size\taverage"

for seq_length in 5 4; do
    for skip_trigger in $(seq 50 5 120); do
        for skip_size in 32 64 128 256 512 1024; do
            {
                average=$(./target/release/cdc-algorithm-tester --input-file data/random_small.bin seq-cdc $seq_length "$skip_trigger" "$skip_size" \
                          | awk -F, '{sum += $2; count++} END {print (count > 0 ? sum/count : "N/A")}')
                echo -e "$seq_length\t$skip_trigger\t$skip_size\t$average"
            } &
        done
    done
done

wait  # Wait for all background processes to complete