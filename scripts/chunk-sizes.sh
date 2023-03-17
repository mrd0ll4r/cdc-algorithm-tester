#!/bin/bash

source scripts/utils.sh

# Get command string for chunker. Pass arguments in the following order: algoname, dataset, target chunk size.
get_cmd() {
  local args="$(get_cmd_args $1 $2 $3)"
  echo "$BIN -i $DATA_PATH/$2 $args"
}

# CSV header
echo "algorithm,dataset,target_chunk_size,chunk_size"

for algo in "${ALGOS[@]}"; do
  readarray -t subalgos < <(get_subalgos "$algo")
  # for each algorithm configuration
  for subalgo in "${subalgos[@]}"; do
    # for each dataset
    for dataset in "${DATASETS[@]}"; do
      # for each target chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        # cut last chunk | get unique chunks | get size
        chunk_sizes=($($(get_cmd $subalgo $dataset $cs) | head -n -1 | awk -F, '!seen[$1]++ {print $2}'))
        # for each produced chunk
        for l in "${chunk_sizes[@]}"; do
            printf "%s,%s,%d,%d\n" $(get_algo_name $subalgo) ${dataset%%.*} $cs $l
        done
      done
    done
  done
done
