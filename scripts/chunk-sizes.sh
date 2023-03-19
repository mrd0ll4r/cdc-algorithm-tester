#!/bin/bash -e

source scripts/utils.sh

# Get command string for chunker. Pass arguments in the following order: algoname, dataset, target chunk size.
get_cmd() {
  local args="$(get_cmd_args "$1" "$2" "$3")"
  echo "$BIN -i $DATA_PATH/$2 $args"
}

# CSV header
echo "algorithm,dataset,target_chunk_size,chunk_size"

for dataset in "${DATASETS[@]}"; do
  dataset_name="${dataset%%.*}"

  for algo in "${ALGOS[@]}"; do
    readarray -t subalgos < <(get_subalgos "$algo")
    for subalgo in "${subalgos[@]}"; do
      subalgo_name=$(get_algo_name "$subalgo")

      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        prefix=$(printf "%s,%s,%d" "$subalgo_name" "$dataset_name" "$cs")
        cmd=$(get_cmd "$subalgo" "$dataset" "$cs")
        # cut last chunk | get unique chunks | get size
        # for each produced chunk
        for l in $($cmd | head -n -1 | sort -u | awk -F, '{print $2}'); do
          echo "$prefix,$l"
        done
      done
    done
  done
done
