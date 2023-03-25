#!/bin/bash -e

source scripts/utils.sh

# Get command string for chunker. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd() {
  local args="$(get_cmd_args "$1" "$2" "$3")"
  echo "$BIN -i $DATA_PATH/$2 $args"
}

# CSV header
echo "algorithm,dataset,dataset_size,target_chunk_size,unique_chunks_size_sum"

for dataset in "${DATASETS[@]}"; do
  dataset_name="${dataset%%.*}"
  dataset_size=$(stat -c "%s" "$DATA_PATH/$dataset")

  for algo in "${ALGOS[@]}"; do
    readarray -t subalgos < <(get_subalgos "$algo")
    for subalgo in "${subalgos[@]}"; do
      algo_name=$(get_algo_name "$subalgo")

      # for each target chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        prefix=$(printf "%s,%s,%d,%d" "$algo_name" "$dataset_name" "$dataset_size" "$cs")
        cmd=$(get_cmd "$subalgo" "$dataset" "$cs")

        # cut last chunk | sum of lengths of all unique chunks
        chunk_size_sum=$($cmd | head -n -1 | sort -u | awk -F, '{print $2}' | paste -sd+ | bc)
        echo "$prefix,$chunk_size_sum"
      done
    done
  done
done
