#!/bin/bash -e

source scripts/utils.sh

# CSV header
echo "algorithm,dataset,dataset_size,target_chunk_size,unique_chunks_size_sum"

for dataset in "${DATASETS[@]}"; do
  dataset_name="${dataset%%.*}"
  dataset_size=$(find "$DATA_PATH/$dataset" -type f -exec stat -c "%s" {} + | awk '{sum+=$1} END {print sum}')

  for algo in "${ALGOS[@]}"; do
    readarray -t subalgos < <(get_subalgos "$algo")
    for subalgo in "${subalgos[@]}"; do
      algo_name=$(get_algo_name "$subalgo")

      # for each target chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        prefix=$(printf "%s,%s,%d,%d" "$algo_name" "$dataset_name" "$dataset_size" "$cs")

        # Run multiple instances and concatenate their outputs
        temp_file=$(mktemp)
        
        # Iterate over each file in the dataset directory
        for file in "$DATA_PATH/$dataset"/*; do
          if [ -f "$file" ]; then
            rel_path="${file#"$DATA_PATH"/}"
            cmd=$(get_cmd "$subalgo" "$rel_path" "$cs")
            $cmd >> "$temp_file"
          fi
        done
        
        chunk_size_sum=$(cat "$temp_file" | sort -u | awk -F, '{print $2}' | paste -sd+ | bc)
        rm "$temp_file"  # Clean up temporary file
        echo "$prefix,$chunk_size_sum"
      done
    done
  done
done
