#!/bin/bash

path=data/web
mkdir -p $path
cd $path

for day in {01..30}
do
  wget -E -H -k -p -l3 -q -P 202206${day} https://web.archive.org/web/202206${day}id_/https://nytimes.com
done