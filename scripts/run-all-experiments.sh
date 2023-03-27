#!/bin/bash -e

mkdir -p csv

########################
# Prepare BFBC
echo "Analyzing data for BFBC..."

make bfbc

########################
# Computational Performance Measurements

echo "Starting computational performance measurements..."

echo "evaluating on random..."
DATASETS="random.bin" make speed | gzip -9 > csv/perf_random.csv.gz

echo "evaluating on code..."
DATASETS="code.tar" make speed | gzip -9 > csv/perf_code.csv.gz

echo "evaluating on web..."
DATASETS="web.tar" make speed | gzip -9 > csv/perf_web.csv.gz

echo "evaluating on pdf..."
DATASETS="pdf.tar" make speed | gzip -9 > csv/perf_pdf.csv.gz

echo "evaluating on LNX..."
DATASETS="lnx.tar" make speed | gzip -9 > csv/perf_lnx.csv.gz

########################
# Chunk size distributions

echo "Starting chunk size distribution measurements..."

bash -c 'DATASETS="random.bin" make csd | gzip -9 > csv/csd_random.csv.gz' &
bash -c 'DATASETS="code.tar" make csd | gzip -9 > csv/csd_code.csv.gz' &
bash -c 'DATASETS="web.tar" make csd | gzip -9 > csv/csd_web.csv.gz' &
bash -c 'DATASETS="pdf.tar" make csd | gzip -9 > csv/csd_pdf.csv.gz' &
bash -c 'DATASETS="lnx.tar" make csd | gzip -9 > csv/csd_lnx.csv.gz' &

########################
# Deduplication ratios

echo "Starting deduplication ratio measurements..."

bash -c 'DATASETS="random.bin" make dedup | gzip -9 > csv/dedup_random.csv.gz' &
bash -c 'DATASETS="code.tar" make dedup | gzip -9 > csv/dedup_code.csv.gz' &
bash -c 'DATASETS="web.tar" make dedup | gzip -9 > csv/dedup_web.csv.gz' &
bash -c 'DATASETS="pdf.tar" make dedup | gzip -9 > csv/dedup_pdf.csv.gz' &
bash -c 'DATASETS="lnx.tar" make dedup | gzip -9 > csv/dedup_lnx.csv.gz' &

########################

sleep 1
echo "Waiting for jobs to finish..."
wait $(jobs -p)

echo "All done."