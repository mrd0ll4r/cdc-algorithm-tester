#!/bin/bash

# Utility function to get the absolute value of a number $1.
abs() {
  if (( $(echo "$1 < 0" | bc -l) )); then
    echo "$1 * -1" | bc -l
  else
    echo $1
  fi
}

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

# Find the right set of divisors to use for BFBC to achieve given target chunk size; $1 is target chunk size, $2 is minimum chunk size, $3 is dataset.
get_divisors_for_bfbc() {
  local target=$1
  local min=$2
  local dataset=$3
  local dataset_size=$(stat -c "%s" $DATA_PATH/$dataset)

  bp_counts=() # bp index => count
  while IFS=, read -r b1 b2 count share; do
    bp_counts+=($count)
  done < <(tail -n +2 "$DATA_PATH/bfbc/$dataset.csv")

  # Get the chunk size for a given set of divisors.
  get_chunk_size() {
    local divisors=("$@")
    local total_chunk_count=1
    for d in "${divisors[@]}"; do
      total_chunk_count=$((total_chunk_count + bp_counts[d]))
    done
    echo "scale=2; ($dataset_size - $min * $total_chunk_count) / $total_chunk_count" | bc
  }

  local divisors=()
  local i=0

  for index in "${!bp_counts[@]}"; do
    local potential_divisors=("${divisors[@]}" "$index")
    local cs=$(get_chunk_size "${potential_divisors[@]}")

    if [ $(echo "$cs >= $target" | bc -l) -eq 1 ]; then
      divisors=("${divisors[@]}" "$index")
      if [ $(echo "$(abs $(echo "$target - $cs" | bc -l)) <= 0.01 * $target" | bc -l) -eq 1 ]; then
        break
      fi
    fi
  done

  if ! echo "${divisors[@]}" | grep -q "\b$index\b"; then
    local potential_divisors=("${divisors[@]}" "$index")
    local potential_distance=$(echo "$target - $(get_chunk_size "${potential_divisors[@]}")" | bc)
    local current_distance=$(echo "$target - $(get_chunk_size "${divisors[@]}")" | bc)
    potential_distance=$(abs $potential_distance)
    current_distance=$(abs $current_distance)
    if [ $(echo "$potential_distance < $current_distance" | bc -l) -eq 1 ]; then
      divisors+=($index)
    fi
  fi

  echo "${divisors[@]}"
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
    "quickcdc")
      subalgos=(
        "--quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 gear" \
        "--quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 gear" \
        "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 gear" \
        "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 gear"
      )
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
    "--quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 gear")
      echo "quick_2"
      ;;
    "--quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 gear")
      echo "quick_3"
      ;;
    "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 gear")
      echo "quick_hash_2"
      ;;
    "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 gear")
      echo "quick_hash_3"
      ;;
    *)
      echo "$subalgo"
      ;;
  esac
}

# Global settings
BIN=./target/release/cdc-algorithm-tester
DATA_PATH=data
FAST_DATA_PATH=/media/ramdisk

if [ -z "${TARGET_CHUNK_SIZES}" ]; then
  TARGET_CHUNK_SIZES=(512 1024 2048 4096 8192)
else
  TARGET_CHUNK_SIZES=($(echo "$TARGET_CHUNK_SIZES" | tr ',' ' '))
fi

if [ -z "${ALGOS}" ]; then
  ALGOS=("fsc" "ae" "ram" "mii" "pci" "rabin" "gear" "nc-gear" "gear64" "quickcdc" "bfbc")
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
