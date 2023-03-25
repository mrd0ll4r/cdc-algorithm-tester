#!/bin/bash

source scripts/utils.sh

# Script to download images of the nytimes.com website for every day of 2022.
# We don't `set -e` on purpose, as some requests to external websites fail.

mkdir -p $DATA_PATH/web
cd $DATA_PATH/web || exit

for month in {01..12}; do
  days_in_month=$(cal -d "2022-${month}-01" | awk 'NF {DAYS = $NF}; END {print DAYS}')
  for day in $(seq -w 1 "$days_in_month"); do
    echo "downloading 2022-$month-$day..."
    wget -E -H -k -p -l3 -q -P "2022${month}${day}" "https://web.archive.org/web/2022${month}${day}id_/https://nytimes.com"
  done
done

echo "Creating TAR archive..."
tar -cf ../web.tar .
cd ..
rm -r web
