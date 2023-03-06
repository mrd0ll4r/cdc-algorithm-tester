#!/bin/bash

random_string() {
  echo "$(date +%s%N)$RANDOM" | md5sum | awk '{print $1}'
}

# delete directories
rm -r data/dlf/*/

# obfuscate file names
find data/dlf -type f | while read FILE; do
  EXTENSION=${FILE##*.}
  mv "$FILE" "$(dirname "$FILE")/$(random_string).$EXTENSION"
done | sort
