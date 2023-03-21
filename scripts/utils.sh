#!/bin/bash -e

# Utility function to get the absolute value of a number $1.
abs() {
  if (($(echo "$1 < 0" | bc -l))); then
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
      "--quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 nc-gear 1"
      "--quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 nc-gear 1"
      "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 nc-gear 1"
      "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 nc-gear 1"
    )
    ;;
  "bfbc")
    subalgos=("bfbc" "bfbc_custom_div") # the name bfbc_custom_div is just a marker and will be evaluated in get_cmd
    ;;
  *)
    subalgos=("$1")
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
  "--quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 nc-gear 1")
    echo "quick_2"
    ;;
  "--quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 nc-gear 1")
    echo "quick_3"
    ;;
  "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 nc-gear 1")
    echo "quick_hash_2"
    ;;
  "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 nc-gear 1")
    echo "quick_hash_3"
    ;;
  "bfbc_custom_div")
    echo "bfbc_custom_div"
    ;;
  *)
    echo "$subalgo"
    ;;
  esac
}

# Get cmd args for execution of algorithm. Pass arguments in the following order: algorithm, dataset, target chunk size.
get_cmd_args() {
  local args_and_algo="$1"
  local params="$3"
  case "$1" in
  "pci")
    params="$(get_w_and_t_for_pci $3)"
    ;;
  "mii")
    args_and_algo="--max-chunk-size $((2 * $3)) $1"
    params="$(get_w_for_mii $3)"
    ;;
  "nop")
    params=""
    ;;
  "quickcdc"*)
    local min=$(($3 / 2))
    args_and_algo="--quickcdc-min-chunk-size $min $1"
    params="$(($3 - $min))"
    ;;
  "bfbc")
    args_and_algo="bfbc chunk --frequency-file $DATA_PATH/bfbc/$2.stats --byte-pair-indices 0 1 2 --min-chunk-size $(($3 - 128))"
    params=""
    ;;
  "bfbc_custom_div")
    local divisors="$(python3 scripts/get-bfbc-divisors.py "$2" "$3" 0)"
    args_and_algo="bfbc chunk --frequency-file $DATA_PATH/bfbc/$2.stats --byte-pair-indices $divisors --min-chunk-size 2"
    params=""
    ;;
  "rabin")
    local w=48
    params="--window-size $w $(($3 - $w))"
    ;;
  esac
  echo "$args_and_algo $params"
}

# Global settings
export BIN=./target/release/cdc-algorithm-tester
export DATA_PATH=data
export FAST_DATA_PATH=fast_data

if [ -z "${TARGET_CHUNK_SIZES}" ]; then
  TARGET_CHUNK_SIZES=(512 1024 2048 4096 8192)
else
  # shellcheck disable=SC2207
  TARGET_CHUNK_SIZES=($(echo "$TARGET_CHUNK_SIZES" | tr ',' ' '))
fi

if [ -z "${ALGOS}" ]; then
  ALGOS=("fsc" "ae" "ram" "mii" "pci" "rabin" "gear" "nc-gear" "gear64" "quickcdc" "bfbc")
  # detect if we are running speed tests
  if [[ "$0" == *"speed"* ]]; then
    ALGOS=("nop" "${ALGOS[@]}")
  fi
else
  # shellcheck disable=SC2207
  ALGOS=($(echo "$ALGOS" | tr ',' ' '))
fi

if [ -z "${DATASETS}" ]; then
  DATASETS=("random.bin" "web.tar" "code.tar" "pdf.tar" "lnx.tar")
else
  # shellcheck disable=SC2207
  DATASETS=($(echo "$DATASETS" | tr ',' ' '))
fi
