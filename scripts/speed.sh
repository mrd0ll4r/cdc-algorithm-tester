#!/bin/bash

source scripts/utils.sh

# Get command string for chunker. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd() {
  local params="$3"
  case "$1" in
    "pci")
      params="$(get_w_and_t_for_pci $3)"
      ;;
    "mii")
      params="$(get_w_for_mii $3)"
      ;;
    "nop")
      params=""
      ;;
  esac
  echo "$BIN -q -i $FAST_DATA_PATH/$2 $1 $params"
}

if [ -z "${ITER}" ]; then
  ITER=10
fi

# clean up on ramdisk from previous runs
for dataset in "${DATASETS[@]}"; do rm -f "$FAST_DATA_PATH/$dataset"; done

# CSV header
echo "algorithm,dataset,dataset_size,target_chunk_size,iteration,time_ms"
echo "algorithm,dataset,dataset_size,target_chunk_size,iteration,event,value" > perf.csv

for algo in "${ALGOS[@]}"; do
  readarray -t subalgos < <(get_subalgos "$algo")

  # run for each sub-algorithm
  for subalgo in "${subalgos[@]}"; do

    for dataset in "${DATASETS[@]}"; do
      dataset_size=$(stat -c "%s" $DATA_PATH/$dataset)

      # copy dataset to ramdisk
      cp $DATA_PATH/$dataset $FAST_DATA_PATH/
      
      # warm-up run
      $(get_cmd $algo $dataset ${TARGET_CHUNK_SIZES[0]}) >/dev/null

      # produce result for each chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        for i in $(seq 1 $ITER); do
          time=$( { time $(get_cmd $subalgo $dataset $cs) >/dev/null ; } 2>&1 | grep real | awk '{print $2}' )
          time=$(time_to_ms $time)
          header=$(printf "%s,%s,%d,%d,%d" $(get_algo_name "$subalgo") ${dataset%%.*} $dataset_size $cs $i)
          echo $header,$time
          perf_result=$( perf stat -x, -e task-clock,context-switches,page-faults,cycles,instructions,branches,branch-misses,L1-dcache-loads,L1-dcache-misses,cache-references,cache-misses $(get_cmd $subalgo $dataset $cs) 2> /dev/stdout 1> /dev/null | awk -F',' '{print $3","$1}')
          for l in $perf_result; do echo $header,$l >> perf.csv; done
        done

        # as nop is agnostic to target chunk sizes, one iteration is enough
        if [ $algo = "nop" ]; then
          break
        fi
      done

      # remove dataset from ramdisk to free space
      rm $FAST_DATA_PATH/$dataset
      if [ $algo = "bfbc" ]; then rm $FAST_DATA_PATH/$dataset.stats; fi
    done
  done
done
