#!/bin/bash -e

source scripts/utils.sh

# CSV header
echo "algorithm,dataset,target_chunk_size,chunk_size"

for dataset in "${DATASETS[@]}"; do
  dataset_name="${dataset%%.*}"

  for algo in "${ALGOS[@]}"; do
    readarray -t subalgos < <(get_subalgos "$algo")
    for subalgo in "${subalgos[@]}"; do
      subalgo_name=$(get_algo_name "$subalgo")

      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        # Iterate over each file in the dataset directory
        for file in "$DATA_PATH/$dataset"/*; do
          if [ -f "$file" ]; then
            rel_path="${file#"$DATA_PATH"/}"
            prefix=$(printf "%s,%s,%d" "$subalgo_name" "$dataset_name" "$cs")
            cmd=$(get_cmd "$subalgo" "$rel_path" "$cs")
            $cmd | awk -v prefix="$prefix" -F, '{print prefix "," $2}'
          fi
        done    
      done
    done
  done
done
