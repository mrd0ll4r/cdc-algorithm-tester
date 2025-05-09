#!/bin/bash -e

source scripts/utils.sh

# CSV header
echo "algorithm,dataset,dataset_size,target_chunk_size,unique_chunks_size_sum"

process_single_file() {
  source scripts/utils.sh
  input_file="$3"
  algo="$1"
  cs="$2"
  rel_path="${input_file#"$DATA_PATH"/}"
  cmd=$(get_cmd "$algo" "$rel_path" "$cs")
  $cmd
}

# Export needed variables/functions for GNU parallel
export -f get_subalgos get_algo_name get_cmd get_cmd_args
export DATA_PATH
export -f process_single_file

for dataset in "${DATASETS[@]}"; do
  dataset_name="${dataset%%.*}"
  dataset_size=$(find "$DATA_PATH/$dataset/" -type f -exec stat -c "%s" {} + | awk '{sum+=$1} END {print sum}')

  for algo in "${ALGOS[@]}"; do
    >&2 echo "working on algo $algo"

    readarray -t subalgos < <(get_subalgos "$algo")
    
    for subalgo in "${subalgos[@]}"; do
      subalgo_name=$(get_algo_name "$subalgo")
      >&2 echo "working on subalgo $subalgo_name"

      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        >&2 echo "working on CS $cs"
        prefix=$(printf "%s,%s,%d,%d" "$algo" "$dataset_name" "$dataset_size" "$cs")

        temp_file=$(mktemp)
        find "$DATA_PATH/$dataset/" -type f |
          parallel process_single_file \""$subalgo"\" \""$cs"\" "{}" |
          sort -S 1048576000 -u |
          awk -F, '{print $2}' |
          paste -sd+ |
          bc > "$temp_file"

        chunk_size_sum=$(cat "$temp_file")
        rm "$temp_file"

        echo "$prefix,$chunk_size_sum"
      done
    done
  done
done