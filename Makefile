# Produces an optimized and fast release build.
build:
	cargo +nightly build --release --locked

# Run speed benchmark on all chunkers.
speed:
	./scripts/speed-benchmark.sh

# Applied on a chunker's CSV output, this will return the average of produced chunk sizes (last chunk is omitted).
avg:
	head -n -1 | awk -F, '{ sum += $2 } END { print sum/NR }'