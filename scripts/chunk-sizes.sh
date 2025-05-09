#!/bin/bash -e

source scripts/utils.sh

# Process a single parameter combination
process_combination() {
    source scripts/utils.sh
    local algo="$1"
    local dataset="$2"
    local cs="$3"
    local file="$4"
    
    local dataset_name="${dataset%%.*}"
    readarray -t subalgos < <(get_subalgos "$algo")
    
    for subalgo in "${subalgos[@]}"; do
        subalgo_name=$(get_algo_name "$subalgo")
        local rel_path="${file#"$DATA_PATH"/}"
        local prefix=$(printf "%s,%s,%d" "$subalgo_name" "$dataset_name" "$cs")
        local cmd=$(get_cmd "$subalgo" "$rel_path" "$cs")
        echo "$cmd" >&2
        $cmd | awk -v prefix="$prefix" -F, '{print prefix "," $2}'
    done
}

# Export required functions and variables for parallel execution
export -f process_combination
export -f get_subalgos
export -f get_algo_name
export -f get_cmd
export -f get_cmd_args
export DATA_PATH

# CSV header
echo "algorithm,dataset,target_chunk_size,chunk_size"

# Find all files for all datasets
all_files=$(find "$DATA_PATH" -type f -path "*/*/[!.]*" | grep -v "/\.")

# Run in parallel with all parameter combinations
parallel --will-cite -j+0 process_combination \
    ::: "${ALGOS[@]}" \
    ::: "${DATASETS[@]}" \
    ::: "${TARGET_CHUNK_SIZES[@]}" \
    ::: "$all_files"
