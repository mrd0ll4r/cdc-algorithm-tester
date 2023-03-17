#!/bin/bash -e

if [ -z "$1" ]; then
    echo "Error: make validate requires the argument <file>"
    exit 1
fi

diff <(cat - | awk -F',' '{print $2}' | paste -sd+ | bc) <(stat -c %s "$1")
if [ $? -eq 0 ]; then \
    echo "Valid"; \
fi