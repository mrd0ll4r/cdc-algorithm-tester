#!/bin/bash -e

source scripts/utils.sh

echo "Working on Rabin..."
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_rabin_16.csv rabin 16 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_rabin_32.csv rabin 32 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_rabin_48.csv rabin 48 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_rabin_64.csv rabin 64 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_rabin_128.csv rabin 128 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_rabin_256.csv rabin 256 8192 >/dev/null

echo "Working on Adler32..."
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_adler32_16.csv adler32 16 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_adler32_32.csv adler32 32 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_adler32_48.csv adler32 48 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_adler32_64.csv adler32 64 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_adler32_128.csv adler32 128 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_adler32_256.csv adler32 256 8192 >/dev/null

echo "Working on Buzhash..."
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_buzhash_16.csv buzhash 16 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_buzhash_32.csv buzhash 32 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_buzhash_48.csv buzhash 48 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_buzhash_64.csv buzhash 64 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_buzhash_128.csv buzhash 128 8192 >/dev/null
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_buzhash_256.csv buzhash 256 8192 >/dev/null

echo "Working on Gear64..."
./out/cdc-algorithm-tester -q -i "$DATA_PATH/random_small.bin" --hash-value-output-file csv/hash_values_rand_small_gear64_64.csv gear64 8192 >/dev/null

echo "Compressing..."
for f in csv/hash_values_*.csv; do
    gzip -9 "$f"
done
