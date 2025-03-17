#!/bin/bash

source scripts/utils.sh

# Script to download images of the nytimes.com website for every day from 2014 to 2024.
# We don't `set -e` on purpose, as some requests to external websites fail.

mkdir -p "$DATA_PATH/web"
cd "$DATA_PATH/web" || exit

year=2024
month=01

days_in_month=$(LC_ALL=C cal -d "${year}-${month}-01" | awk 'NF {DAYS = $NF}; END {print DAYS}')
for day in $(seq -w 1 "$days_in_month"); do
  echo "downloading https://web.archive.org/web/${year}${month}${day}id_/https://nytimes.com"
  wget -E -H -k -p -r -l1 -q --timeout=30 --tries=2 -P "${year}${month}${day}" "https://web.archive.org/web/${year}${month}${day}id_/https://nytimes.com" &
done

wait

for day in $(seq -w 1 "$days_in_month"); do
  tar -cf "${year}${month}${day}.tar" "${year}${month}${day}"
  rm -r "${year}${month}${day}"
done
