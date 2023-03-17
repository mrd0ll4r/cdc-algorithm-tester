#!/bin/bash

source scripts/utils.sh

# Get command string for chunker. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd() {
  local args="$(get_cmd_args $1 $2 $3)"
  echo "$BIN -i $DATA_PATH/$2 $args"
}

# CSV header
echo "algorithm,dataset,target_chunk_size,dedup_ratio"

for algo in "${ALGOS[@]}"; do
  readarray -t subalgos < <(get_subalgos "$algo")
  # for each algorithm configuration
  for subalgo in "${subalgos[@]}"; do
    # for each dataset
    for dataset in "${DATASETS[@]}"; do
      # for each target chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        # cut last chunk | 1 - (sum of lengths of unique chunks) / (sum of lengths of all chunks)
        dedup_ratio=($($(get_cmd $subalgo $dataset $cs) | head -n -1 | awk -F, '{sums[$1]=$2; total+=$2} END {for (f in sums) {unique_sum+=sums[f]; count++}; printf "%.5f\n", 1-unique_sum/total}'))
        printf "%s,%s,%d,%s\n" $(get_algo_name $subalgo) ${dataset%%.*} $cs $dedup_ratio
      done
    done
  done
done
