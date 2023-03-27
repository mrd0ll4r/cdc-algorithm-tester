#!/bin/bash -e

source scripts/utils.sh

perf_events_to_collect="task-clock,context-switches,page-faults,cycles,instructions,branches,branch-misses,L1-dcache-loads,L1-dcache-misses,L1-dcache-prefetches,cache-references,cache-misses"

# Get command string for chunker. Pass arguments in the following order: algoname, dataset, target chunk size.
get_cmd() {
  local args="$(get_cmd_args "$1" "$2" "$3")"
  echo "$BIN -q -i $FAST_DATA_PATH/$2 $args"
}

if [ -z "${ITER}" ]; then
  ITER=10
fi

# clean up on ramdisk from previous runs
#for dataset in "${DATASETS[@]}"; do
#  rm -f "$FAST_DATA_PATH/$dataset" "$FAST_DATA_PATH/$dataset.stats"
#done

# CSV header
echo "algorithm,dataset,dataset_size,target_chunk_size,iteration,event,value"

for dataset in "${DATASETS[@]}"; do
  # copy dataset to ramdisk
  cp "$DATA_PATH/$dataset" "$FAST_DATA_PATH/"

  dataset_size=$(stat -c "%s" "$FAST_DATA_PATH/$dataset")

  for algo in "${ALGOS[@]}"; do
    readarray -t subalgos < <(get_subalgos "$algo")
    for subalgo in "${subalgos[@]}"; do

      # warm-up run
      $(get_cmd "$subalgo" "$dataset" "${TARGET_CHUNK_SIZES[0]}") >/dev/null

      # produce result for each chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        cmd=$(get_cmd "$subalgo" $dataset $cs)
        for i in $(seq 1 $ITER); do
          header=$(printf "%s,%s,%d,%d,%d" $(get_algo_name "$subalgo") "${dataset%%.*}" "$dataset_size" "$cs" "$i")
          perf_result=$(perf stat -x, -e "$perf_events_to_collect" $cmd 2>/dev/stdout 1>/dev/null | awk -F',' '{print $3","$1}')
          for l in $perf_result; do echo "$header,$l"; done
        done

        # as nop is agnostic to target chunk sizes, one chunk size is enough
        if [ "$algo" = "nop" ]; then
          break
        fi
      done
    done
  done
  # remove dataset from ramdisk to free space
  rm "$FAST_DATA_PATH/$dataset"
done
