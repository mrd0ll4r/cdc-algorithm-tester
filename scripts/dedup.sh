#!/bin/bash -e

source scripts/utils.sh

# Get command string for chunker. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd() {
  local args="$(get_cmd_args "$1" "$2" "$3")"
  echo "$BIN -i $DATA_PATH/$2 $args"
}

# CSV header
echo "algorithm,dataset,target_chunk_size,dedup_ratio"

for dataset in "${DATASETS[@]}"; do
  dataset_name="${dataset%%.*}"

  for algo in "${ALGOS[@]}"; do
    readarray -t subalgos < <(get_subalgos "$algo")
    for subalgo in "${subalgos[@]}"; do
      algo_name=$(get_algo_name "$subalgo")

      # for each target chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        prefix=$(printf "%s,%s,%d" "$algo_name" "$dataset_name" "$cs")
        cmd=$(get_cmd "$subalgo" "$dataset" "$cs")

        # cut last chunk | 1 - (sum of lengths of unique chunks) / (sum of lengths of all chunks)
        dedup_ratio=$($cmd | head -n -1 | awk -F, '{sums[$1]=$2; total+=$2} END {for (f in sums) {unique_sum+=sums[f]; count++}; printf "%.5f\n", 1-unique_sum/total}')
        echo "$prefix,$dedup_ratio"
      done
    done
  done
done
