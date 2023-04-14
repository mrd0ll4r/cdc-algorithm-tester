#!/bin/bash -e

# Finds the best match for parameter w in MII to achieve given target chunk size.
get_w_for_mii() {
  MII_AVGS=(1 2 4 9 28 126 737 5152 41448 375910 3792352) # mapping from w to average chunk size for w=0..10
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
    echo "62 270"
    ;;
  1024)
    echo "62 274"
    ;;
  2048)
    echo "60 269"
    ;;
  4096)
    echo "61 276"
    ;;
  8192)
    echo "62 283"
    ;;
  737)
    echo "60 264"
    ;;
  5152)
    echo "61 277"
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
      "--quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 rabin 64"
      "--quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 rabin 64"
      "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 rabin 64"
      "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 rabin 64"
    )
    ;;
  "rabin")
    subalgos=(
      "rabin 16"
      "rabin 32"
      "rabin 48"
      "rabin 64"
      "rabin 128"
      "rabin 256"
    )
    ;;
  "adler32")
    subalgos=(
      "adler32 16"
      "adler32 32"
      "adler32 48"
      "adler32 64"
      "adler32 128"
      "adler32 256"
    )
    ;;
  "buzhash")
    subalgos=(
      "buzhash 16"
      "buzhash 32"
      "buzhash 48"
      "buzhash 64"
      "buzhash 128"
      "buzhash 256"
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
  case "$1" in
  "nc-gear"*)
    local level=$(echo "$1" | awk -F' ' '{print $2}')
    echo "gear_nc_$level"
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
  "--quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 rabin 64")
    echo "quick_2_rabin_64"
    ;;
  "--quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 rabin 64")
    echo "quick_3_rabin_64"
    ;;
  "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 2 --quickcdc-end-feature-vector-length 2 rabin 64")
    echo "quick_hash_2_rabin_64"
    ;;
  "--quickcdc-use-hashmap --quickcdc-front-feature-vector-length 3 --quickcdc-end-feature-vector-length 3 rabin 64")
    echo "quick_hash_3_rabin_64"
    ;;
  "bfbc_custom_div")
    echo "bfbc_custom_div"
    ;;
  "rabin"*)
    echo "${1/ /_}"
    ;;
  "adler32"*)
    echo "${1/ /_}"
    ;;
  "buzhash"*)
    echo "${1/ /_}"
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
    local divisors="$(python3 scripts/get-bfbc-divisors.py "$2" "$3" 2)"
    args_and_algo="bfbc chunk --frequency-file $DATA_PATH/bfbc/$2.stats --byte-pair-indices $divisors --min-chunk-size 2"
    params=""
    ;;
  esac
  echo "$args_and_algo $params"
}

# Global settings
export BIN=./target/release/cdc-algorithm-tester
export DATA_PATH=data
export FAST_DATA_PATH=fast_data

if [ -z "${TARGET_CHUNK_SIZES}" ]; then
  TARGET_CHUNK_SIZES=(512 1024 2048 4096 8192 737 5152)
else
  # shellcheck disable=SC2207
  TARGET_CHUNK_SIZES=($(echo "$TARGET_CHUNK_SIZES" | tr ',' ' '))
fi

if [ -z "${ALGOS}" ]; then
  ALGOS=("fsc" "ae" "ram" "mii" "pci" "rabin" "adler32" "buzhash" "gear" "nc-gear" "gear64" "quickcdc" "bfbc")
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

mkdir -p "$DATA_PATH"
mkdir -p "$FAST_DATA_PATH"
