#!/bin/bash -e

mkdir -p csv

########################
# Prepare BFBC
echo "Analyzing data for BFBC..."

make bfbc

########################
# Computational Performance Measurements

echo "Running computational performance measurements. This will take a while."

echo "evaluating on random..."
DATASETS="random.bin" make speed | gzip -9 > csv/perf_random.csv.gz

echo "evaluating on empty..."
DATASETS="empty.bin" make speed | gzip -9 > csv/perf_empty.csv.gz

echo "evaluating on zero..."
DATASETS="zero.bin" make speed | gzip -9 > csv/perf_zero.csv.gz

echo "evaluating on code..."
DATASETS="code.tar" make speed | gzip -9 > csv/perf_code.csv.gz

echo "evaluating on web..."
DATASETS="web.tar" make speed | gzip -9 > csv/perf_web.csv.gz

echo "evaluating on pdf..."
DATASETS="pdf.tar" make speed | gzip -9 > csv/perf_pdf.csv.gz

echo "evaluating on LNX..."
DATASETS="lnx.tar" make speed | gzip -9 > csv/perf_lnx.csv.gz

# perf doesn't count some events if the execution doesn't last long enough.
# This happens for the empty dataset.
gunzip csv/perf_empty.csv.gz
sed -i '/counted>/d' csv/perf_empty.csv
sed -i '/<not/d' csv/perf_empty.csv
gzip -9 csv/perf_empty.csv

########################
# File sizes

echo "Analyzing file sizes in parallel..."

bash -c 'DATASET="code.tar" make sizes > csv/sizes_code.txt' &
bash -c 'DATASET="web.tar" make sizes > csv/sizes_web.txt' &
bash -c 'DATASET="pdf.tar" make sizes > csv/sizes_pdf.txt' &
bash -c 'DATASET="lnx.tar" make sizes > csv/sizes_lnx.txt' &

########################
# Chunk size distributions
# We do not evaluate this on the empty dataset.

echo "Starting chunk size distribution measurements in parallel..."

bash -c 'DATASETS="random.bin" make csd | gzip -9 > csv/csd_random.csv.gz' &
bash -c 'DATASETS="zero.bin" make csd | gzip -9 > csv/csd_zero.csv.gz' &
bash -c 'DATASETS="code.tar" make csd | gzip -9 > csv/csd_code.csv.gz' &
bash -c 'DATASETS="web.tar" make csd | gzip -9 > csv/csd_web.csv.gz' &
bash -c 'DATASETS="pdf.tar" make csd | gzip -9 > csv/csd_pdf.csv.gz' &
bash -c 'DATASETS="lnx.tar" make csd | gzip -9 > csv/csd_lnx.csv.gz' &

########################
# Deduplication ratios
# We do not evaluate this on the empty dataset.

echo "Starting deduplication ratio measurements in parallel..."

bash -c 'DATASETS="random.bin" make dedup | gzip -9 > csv/dedup_random.csv.gz' &
bash -c 'DATASETS="zero.bin" make dedup | gzip -9 > csv/dedup_zero.csv.gz' &
bash -c 'DATASETS="code.tar" make dedup | gzip -9 > csv/dedup_code.csv.gz' &
bash -c 'DATASETS="web.tar" make dedup | gzip -9 > csv/dedup_web.csv.gz' &
bash -c 'DATASETS="pdf.tar" make dedup | gzip -9 > csv/dedup_pdf.csv.gz' &
bash -c 'DATASETS="lnx.tar" make dedup | gzip -9 > csv/dedup_lnx.csv.gz' &

########################
# Hash value distributions

echo "Starting hash value distribution experiments in the background..."
./scripts/hash-value-distributions.sh &

########################

sleep 1
echo "Waiting for jobs to finish..."
wait $(jobs -p)

########################

echo "Splitting chunk size distribution measurements by algorithm..."

for f in csv/csd_*.csv.gz; do
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
