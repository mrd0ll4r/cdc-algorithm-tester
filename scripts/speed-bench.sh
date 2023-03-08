#!/bin/bash

# Convert time string to milliseconds (e.g., "1m23.456s" -> 82345).
time_to_ms() {
  local time="$1"
  local minutes=$(echo "$time" | sed 's/^ *\([0-9]*\)m.*$/\1/' | sed 's/^0*//')
  local seconds=$(echo "$time" | sed 's/^ *[0-9]*m\([0-9]*\.[0-9]*\)s$/\1/' | cut -d'.' -f1 | sed 's/^0*//')
  local milliseconds=$(echo "$time" | sed 's/^ *[0-9]*m[0-9]*\.\([0-9]*\)s$/\1/' | sed 's/^0*//')
  echo "$((minutes * 60 * 1000 + seconds * 1000 + milliseconds))"
}

# Get command string for chunker. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd() {
  echo "$BIN --quiet --input-file $FAST_DATA_PATH/$2 $1 $3"
}

# Global settings
ITER=10
CS_RANGE_START=4096
CS_RANGE_END=CS_RANGE_START*2*2*2*2
BIN=./target/release/cdc-algorithm-tester
ALGOS=("ae" "ram" "bfbc" "mii" "pci" "gear" "nc-gear" "gear64")
DATASETS=("random.bin" "web.tar" "code.tar" "pdf.tar" "lnx.tar")
DATA_PATH=/media/ramdisk
FAST_DATA_PATH=/media/ramdisk

# clean up on ramdisk from previous runs
for dataset in "${DATASETS[@]}"; do rm "$FAST_DATA_PATH/$dataset"; done

for algo in "${ALGOS[@]}"
do
  # warm-up run
  $(get_cmd $algo random.bin $CS_RANGE_START)

  for dataset in "${DATASETS[@]}"
  do
    cp $DATA_PATH/$dataset $FAST_DATA_PATH/
    for (( cs=CS_RANGE_START; cs<=CS_RANGE_END; cs=cs*2 )); do
        time_sum=$(
            (
                time for a in $(seq 1 $ITER); do
                    $(get_cmd $algo $dataset $cs)
                done
            ) 2>&1 >/dev/null | grep real | awk '{print $2}'
        )
        time_in_ms=$(time_to_ms $time_sum)
        time_avg=$(echo "$time_in_ms / $ITER" | bc)
        printf "%s,%s,%d,%s\n" $algo ${dataset%%.*} $cs $time_avg
    done
    rm $FAST_DATA_PATH/$dataset
  done
done
