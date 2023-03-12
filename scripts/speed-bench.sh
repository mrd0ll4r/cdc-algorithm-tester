#!/bin/bash

# Convert time string to milliseconds (e.g., "1m23.456s" -> 82345).
time_to_ms() {
  local time="$1"
  local minutes=$(echo "$time" | sed 's/^ *\([0-9]*\)m.*$/\1/' | sed 's/^0*//')
  local seconds=$(echo "$time" | sed 's/^ *[0-9]*m\([0-9]*\.[0-9]*\)s$/\1/' | cut -d'.' -f1 | sed 's/^0*//')
  local milliseconds=$(echo "$time" | sed 's/^ *[0-9]*m[0-9]*\.\([0-9]*\)s$/\1/' | sed 's/^0*//')
  echo "$((minutes * 60 * 1000 + seconds * 1000 + milliseconds))"
}

MII_AVGS=(1 1 2 6 24 121 731 5145 41440 375901 3792342)

# Finds the best match for parameter w in MII to achieve given target chunk size.
get_w_for_mii() {
    MII_AVGS=(1 1 2 6 24 121 731 5145 41440 375901 3792342) # mapping from w to average chunk size for w=0..10
    target=$1
    closest=0
    distance=$(($target - ${MII_AVGS[0]}))
    for i in "${!MII_AVGS[@]}"; do
        current=${MII_AVGS[$i]}
        if [ $current -eq $target ]; then
            echo $i
            return
        fi
        current_distance=$(($target - $current))
        if [ $current_distance -lt 0 ]; then
            current_distance=$(($current_distance * -1))
        fi
        if [ $current_distance -lt $distance ]; then
            closest=$i
            distance=$current_distance
        fi
    done
    echo $closest
}

# Finds the best match for parameters w and t in PCI to achieve given target chunk size.
get_w_and_t_for_pcii() {
  case $1 in
    512)
        echo "342 776"
        ;;
    1024)
        echo "603 2672"
        ;;
    2048)
        echo "285 1344"
        ;;
    4096)
        echo "2 15"
        ;;
    8192)
        echo "88 396"
        ;;
    *)
        echo "Error: Target chunk size not supported for PCI."
        exit 1
        ;;
  esac
}

# Get command string for chunker. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd() {
  local params="$3"
  if [ $3 = "pci" ]; then
    params="$(get_w_and_t_for_pci $3)"
  elif [ $3 = "mii" ]; then
    params="$(get_w_for_mii $3)"
  fi
  echo "$BIN -q -i $FAST_DATA_PATH/$2 $1 $params"
}

# Global settings
BIN=./target/release/cdc-algorithm-tester
DATA_PATH=data
FAST_DATA_PATH=/media/ramdisk

if [ -z "${ITER}" ]; then
  ITER=10
fi

if [ -z "${TARGET_CHUNK_SIZES}" ]; then
  TARGET_CHUNK_SIZES=(512 1024 2048 4096 8192)
else
  TARGET_CHUNK_SIZES=($(echo "$TARGET_CHUNK_SIZES" | tr ',' ' '))
fi

if [ -z "${ALGOS}" ]; then
  ALGOS=("ae" "ram" "mii" "pci" "gear" "nc-gear" "gear64" "bfbc")
else
  ALGOS=($(echo "$ALGOS" | tr ',' ' '))
fi

if [ -z "${DATASETS}" ]; then
  DATASETS=("random.bin" "web.tar" "code.tar" "pdf.tar" "lnx.tar")
else
  DATASETS=($(echo "$DATASETS" | tr ',' ' '))
fi

# clean up on ramdisk from previous runs
for dataset in "${DATASETS[@]}"; do rm -f "$FAST_DATA_PATH/$dataset"; done

# CSV header
echo "algorithm,dataset,dataset_size,target_chunk_size,iteration,time_ms"

for algo in "${ALGOS[@]}"; do
  # expand algorithm varieties
  case $algo in
    "nc-gear")
      subalgos=("nc-gear --level=1" "nc-gear --level=2" "nc-gear --level=3")
      ;;
    "gear64")
      subalgos=("gear64" "gear64 --allow-simd-impl")
      ;;
    "bfbc")
      for dataset in "${DATASETS[@]}"; do $(get_cmd "bfbc" "$dataset" "analyze $FAST_DATA_PATH/$dataset.stats"); done
      subalgos=("bfbc chunk $FAST_DATA_PATH/$dataset.stats 0 0 0") # TODO
      ;;
    *)
      subalgos=($algo)
      ;;
  esac

  # run for each sub-algorithm
  for subalgo in "${subalgos[@]}"; do

    # set algo names
    case $subalgo in
      "nc-gear --level=1")
        algoname="gear_nc_1"
        ;;
      "nc-gear --level=2")
        algoname="gear_nc_2"
        ;;
      "nc-gear --level=3")
        algoname="gear_nc_3"
        ;;
      "gear64 --allow-simd-impl")
        algoname="gear64_simd"
        ;;
      *)
        algoname=$subalgo
        ;;
    esac

    for dataset in "${DATASETS[@]}"; do
      dataset_size=$(stat -c "%s" $DATA_PATH/$dataset)

      # copy dataset to ramdisk
      cp $DATA_PATH/$dataset $FAST_DATA_PATH/
      
      # warm-up run
      $(get_cmd $algo $dataset $CS_RANGE_START) >/dev/null

      # produce result for each chunk size
      for cs in "${TARGET_CHUNK_SIZES[@]}"; do
        for i in $(seq 1 $ITER); do
          time=$( { time $(get_cmd $subalgo $dataset $cs) >/dev/null ; } 2>&1 | grep real | awk '{print $2}' )
          printf "%s,%s,%d,%d,%d,%s\n" $algoname ${dataset%%.*} $dataset_size $cs $i $(time_to_ms $time)
        done
      done

      # remove dataset from ramdisk to free space
      rm $FAST_DATA_PATH/$dataset
      if [ $algo = "bfbc" ]; then rm $FAST_DATA_PATH/$dataset.stats; fi
    done
  done
done
