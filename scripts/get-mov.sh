#!/bin/bash

# This script requires `apt install youtube-dl`.

declare -a ids=(
  "sT1m1sRIL2U" "SiDTEwnrjD0" "6ZPdY6pxWTE" "127rJwowaAc" "OJ8fsE_GOlg" "bP4XXWfcXJU" "N43RxHQnetM"
  "Fu3AFdBGY2o" "6-MlQHU-n8M" "l49dPzErMK4" "7V6AeZfFQ5U" "xBYune0GZK8" "Ll_zxG_bIOs" "zcFa6Ek1nFI"
)
ids=($(for v in "${ids[@]}"; do echo "$v";done| sort -u))

for id in "${ids[@]}"
do
   youtube-dl https://www.youtube.com/watch?v=$id -o "data/mov/%(id)s.%(ext)s"
done
