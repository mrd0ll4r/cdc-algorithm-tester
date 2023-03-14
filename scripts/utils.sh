#!/bin/bash

# Convert time string to milliseconds (e.g., "1m23.456s" -> 82345).
time_to_ms() {
  local time="$1"
  local minutes=$(echo "$time" | sed 's/^ *\([0-9]*\)m.*$/\1/' | sed 's/^0*//')
  local seconds=$(echo "$time" | sed 's/^ *[0-9]*m\([0-9]*\.[0-9]*\)s$/\1/' | cut -d'.' -f1 | sed 's/^0*//')
  local milliseconds=$(echo "$time" | sed 's/^ *[0-9]*m[0-9]*\.\([0-9]*\)s$/\1/' | sed 's/^0*//')
  echo "$((minutes * 60 * 1000 + seconds * 1000 + milliseconds))"
}

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
get_w_and_t_for_pci() {
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

get_subalgos() {
  local subalgos=()
  case $1 in
    "nc-gear")
      subalgos=("nc-gear 1" "nc-gear 2" "nc-gear 3")
      ;;
    "gear64")
      subalgos=("gear64" "gear64 --allow-simd-impl")
      ;;
    "bfbc")
      for dataset in "${DATASETS[@]}"; do $(get_cmd "bfbc" "$dataset" "analyze $FAST_DATA_PATH/$dataset.stats"); done
      subalgos=("bfbc chunk $FAST_DATA_PATH/$dataset.stats 0 0 0") # TODO
      ;;
    *)
      subalgos=($1)
      ;;
  esac
  for subalgo in "${subalgos[@]}"; do echo $subalgo; done
}

get_algo_name() {
  case $1 in
    "nc-gear 1")
      echo "gear_nc_1"
      ;;
    "nc-gear 2")
      echo "gear_nc_2"
      ;;
    "nc-gear 3")
      echo "gear_nc_3"
      ;;
    "gear64 --allow-simd-impl")
      echo "gear64_simd"
      ;;
    *)
      echo "$subalgo"
      ;;
  esac
}

# Global settings
BIN=./target/release/cdc-algorithm-tester
DATA_PATH=data
FAST_DATA_PATH=fast_data

if [ -z "${TARGET_CHUNK_SIZES}" ]; then
  TARGET_CHUNK_SIZES=(512 1024 2048 4096 8192)
else
  TARGET_CHUNK_SIZES=($(echo "$TARGET_CHUNK_SIZES" | tr ',' ' '))
fi

if [ -z "${ALGOS}" ]; then
  ALGOS=("fsc" "ae" "ram" "mii" "pci" "gear" "nc-gear" "gear64" "bfbc")
  # detect if we are running speed tests
  if [[ "$0" == *"speed"* ]]; then
    ALGOS=("nop" "${ALGOS[@]}")
  fi
else
  ALGOS=($(echo "$ALGOS" | tr ',' ' '))
fi

if [ -z "${DATASETS}" ]; then
  DATASETS=("random.bin" "web.tar" "code.tar" "pdf.tar" "lnx.tar")
else
  DATASETS=($(echo "$DATASETS" | tr ',' ' '))
fi
