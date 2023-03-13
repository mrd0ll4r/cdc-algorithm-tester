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
          printf "%s,%s,%d,%d,%d,%s\n" $(get_algo_name $subalgo) ${dataset%%.*} $dataset_size $cs $i $(time_to_ms $time)
        done
      done

      # remove dataset from ramdisk to free space
      rm $FAST_DATA_PATH/$dataset
      if [ $algo = "bfbc" ]; then rm $FAST_DATA_PATH/$dataset.stats; fi
    done
  done
done
