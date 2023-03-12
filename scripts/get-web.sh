#!/bin/bash

path=data/web
mkdir -p $path
cd $path

for month in {01..12}
do
  days_in_month=$(cal -d "2022-${month}-01" | awk 'NF {DAYS = $NF}; END {print DAYS}')
  for day in $(seq -w 1 $days_in_month)
  do
    wget -E -H -k -p -l3 -q -P 2022${month}${day} https://web.archive.org/web/2022${month}${day}id_/https://nytimes.com
  done
done

tar -cf ../web.tar .
cd .. && rm -r web