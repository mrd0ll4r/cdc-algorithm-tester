#!/bin/bash -e

mkdir -p csv

########################
# Prepare BFBC Divisors
make bfbc

########################
# Computational Performance Measurements

echo "evaluating on random..."
DATASETS="random.bin" make speed > csv/time_random.csv
mv perf.csv csv/perf_random.csv

echo "evaluating on code..."
DATASETS="code.cat" make speed > csv/time_code.csv
mv perf.csv csv/perf_code.csv

echo "evaluating on web..."
DATASETS="web.cat" make speed > csv/time_web.csv
mv perf.csv csv/perf_web.csv

echo "evaluating on pdf..."
DATASETS="pdf.cat" make speed > csv/time_pdf.csv
mv perf.csv csv/perf_pdf.csv

echo "evaluating on LNX..."
DATASETS="lnx.cat" make speed > csv/time_lnx.csv
mv perf.csv csv/perf_lnx.csv

echo "compressing..."
gzip -9 csv/time_*.csv
gzip -9 csv/perf_*.csv

########################
# File sizes

echo "Analyzing file sizes in parallel..."

bash -c 'DATASET="code.tar" make sizes > csv/sizes_code.txt' &
bash -c 'DATASET="web.tar" make sizes > csv/sizes_web.txt' &
bash -c 'DATASET="pdf.tar" make sizes > csv/sizes_pdf.txt' &
bash -c 'DATASET="lnx.tar" make sizes > csv/sizes_lnx.txt' &

########################
# Chunk size distributions

echo "Starting chunk size distribution measurements..."

bash -c 'DATASETS="random.bin" make csd | gzip -9 > csv/csd_random.csv.gz' &
bash -c 'DATASETS="code.cat" make csd | gzip -9 > csv/csd_code.csv.gz' &
bash -c 'DATASETS="web.cat" make csd | gzip -9 > csv/csd_web.csv.gz' &
bash -c 'DATASETS="pdf.cat" make csd | gzip -9 > csv/csd_pdf.csv.gz' &
bash -c 'DATASETS="lnx.cat" make csd | gzip -9 > csv/csd_lnx.csv.gz' &

########################
# Deduplication ratios

echo "Starting deduplication ratio measurements..."

bash -c 'DATASETS="random.bin" make dedup | gzip -9 > csv/dedup_random.csv.gz' &
bash -c 'DATASETS="code.cat" make dedup | gzip -9 > csv/dedup_code.csv.gz' &
bash -c 'DATASETS="web.cat" make dedup | gzip -9 > csv/dedup_web.csv.gz' &
bash -c 'DATASETS="pdf.cat" make dedup | gzip -9 > csv/dedup_pdf.csv.gz' &
bash -c 'DATASETS="lnx.cat" make dedup | gzip -9 > csv/dedup_lnx.csv.gz' &

########################
# Hash value distributions

echo "Starting hash value distribution experiments in the background..."
./scripts/hash-value-distributions.sh &

########################

sleep 1
echo "Waiting for jobs to finish..."
wait $(jobs -p)

########################

echo "Creating a copy of the chunk size distribution measurements filtered by target_chunk_size=737..."

for f in csv/csd_*.csv.gz; do
        # skip algorithm specific files (should not exist a this point but just to be sure)
        if [[ "$f" == csv/csd_*_*.csv.gz ]]; then
                continue
        fi
        echo "filtering $f..."
        header=$(zcat "$f" | head -n 1)
        b=$(basename "$f" ".csv.gz")
        output_file="csv/${b}_737.csv"
        echo "output to $output_file"
        echo "$header" > "$output_file"
        { zcat "$f" | awk -v output_file="$output_file" -F ',' '$3 == 737 {print >> output_file}'; } >/dev/null 2>&1
        echo "compressing..."
        gzip -9 "$output_file"
done

echo "Splitting chunk size distribution measurements by algorithm..."

for f in csv/csd_*.csv.gz; do
        if [[ "$f" == *_737.csv.gz ]]; then
                continue
        fi
        echo "splitting $f..."
        header=$(zcat "$f" | head -n 1)
        b=$(basename "$f" ".csv.gz")
        filter="{print > (\"csv/${b}_\" \$1 \".csv\")}"
        zcat "$f" | awk "$filter" FS=,

        # Clean up the empty _algorithm file produced (from the header)
        rm "csv/${b}_algorithm.csv"

        echo "compressing..."
        for ff in csv/${b}_*.csv; do
                # prepend the header
                sed -i -e "1i $header" "$ff"
                gzip -9 "$ff"
        done
done


echo "All done."
