# Produces an optimized and fast release build.
build:
	cargo +nightly build --release --locked

# Run speed benchmark on all chunkers.
speed:
	./scripts/speed-bench.sh

# Get average of produced chunk sizes (last chunk is omitted).
avg:
	head -n -1 | awk -F, '{ sum += $2 } END { print sum/NR }'

# Sanity check to validate that the blocks add up to the original file.
validate:
	awk -F',' '{print $2}' | paste -sd+ | bc