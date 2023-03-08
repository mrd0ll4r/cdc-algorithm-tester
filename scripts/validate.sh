#!/bin/bash

diff <(cat - | awk -F',' '{print $2}' | paste -sd+ | bc) <(stat -c %s $1)
if [ $? -eq 0 ]; then \
    echo "Valid"; \
fi