#!/bin/bash

time_to_ms() {
  local time="$1"
  local minutes=$(echo "$time" | sed 's/^ *\([0-9]*\)m.*$/\1/')
  local seconds=$(echo "$time" | sed 's/^ *[0-9]*m\([0-9]*\.[0-9]*\)s$/\1/' | cut -d'.' -f1)
  local milliseconds=$(echo "$time" | sed 's/^ *[0-9]*m[0-9]*\.\([0-9]*\)s$/\1/')
  echo "$((minutes * 60 * 1000 + seconds * 1000 + milliseconds))"
}

ITER=10
RAND_FILE=$(openssl rand 1000000)
CS_RANGE_START=4096
CS_RANGE_END=CS_RANGE_START*2*2*2*2
BIN=./target/release/cdc-algorithm-tester
ALGOS=("ae" "ram" "bfbc" "mii" "pci" "gear" "nc-gear" "gear64")

#--input-file data/web.tar ae 2000
for algo in "${ALGOS[@]}"
do
  for (( cs=CS_RANGE_START; cs<=CS_RANGE_END; cs=cs*2 )); do
      time=$( (time for a in {1..ITER}; do echo $RAND_FILE | $BIN $algo $cs > /dev/null; done) 2>&1 >/dev/null | grep real | awk '{print $2}' )
      time_in_ms=$(time_to_ms $time)
      printf "FSC,%d,%s\n" $cs $time_in_ms
  done
done
