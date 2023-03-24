#!/bin/bash

source scripts/utils.sh

first_iter=true

for dataset in "${DATASETS[@]}"; do
    res=$(ent -t "$DATA_PATH/$dataset")

    if [ "$first_iter" = true ] ; then
        echo "dataset,$(echo "$res" | head -n 1 | cut -c 3-)"
        first_iter=false
    fi

    echo "$dataset,$(echo "$res" | tail -n 1 | cut -c 3-)"
done
