#!/bin/bash -e

wget -U "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0" \
  -r -l 10 -nd -e robots=off -t 3 -T 5 -P ./data/pdf -A ".pdf" -H https://www.semanticscholar.org/
