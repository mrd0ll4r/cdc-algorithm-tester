#!/bin/sh

# Check if at least one numeric arg is passed
if [ $# -eq 0 ]
then
    echo "Usage: $0 <seconds>"
    exit 1
fi

# Read and discard input for 1 second
timeout $1 cat > /dev/null

# Read and print the rest of the input
cat