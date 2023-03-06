#!/bin/bash

wget -U "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:49.0) Gecko/20100101 Firefox/49.0" \
  -nd -r --level=5 -e robots=off -P ./data/jpeg -A ".jpg,.jpeg" -H https://boards.4channel.org/g/2
