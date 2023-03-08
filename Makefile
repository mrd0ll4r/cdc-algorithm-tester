.SILENT:

# Produces an optimized and fast release build.
build:
	cargo +nightly build --release --locked

# Run speed benchmark on all chunkers.
speed:
	./scripts/speed-bench.sh

# Get average of produced chunk sizes (last chunk is omitted).
# Usage: cdc-algorithm-tester ... | make avg
avg:
	head -n -1 | awk -F, '{ sum += $$2 } END { print sum/NR }'

# Validate that the produced chunks add up to the original file size.
# Usage: cdc-algorithm-tester --input-file <input-file> ... | make validate <input-file>
validate:
	./scripts/validate.sh $(word 2, $(MAKECMDGOALS))
