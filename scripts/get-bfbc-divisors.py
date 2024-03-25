#!/usr/bin/env python3

import sys
import os
import csv

if len(sys.argv) != 4:
    raise ValueError("Invalid number of arguments. Usage: python get-bfbc-divisors.py <dataset> <target> <min>")

def read_csv(path):
    """
    Load dataset stats csv into an array of objects (the top byte pairs).
    """
    stats = []
    with open(path, newline='') as csvfile:
        reader = csv.reader(csvfile)
        header = next(reader)
        for row in reader:
            obj = {}
            for i in range(len(header)):
                value = row[i]
                if header[i] in ['byte1', 'byte2', 'count']:
                    value = int(value)
                elif header[i] == 'share':
                    value = float(value)
                obj[header[i]] = value
            stats.append(obj)
    return stats

data_path = os.environ.get('DATA_PATH') or 'data'
dataset = sys.argv[1]
dataset_size = os.stat(f'{data_path}/{dataset}').st_size
target = int(sys.argv[2])
min = int(sys.argv[3])
divisors = []
cs = dataset_size # expected average chunk size with current set of divisors
byte_pairs = list(map(lambda x: x['count'], read_csv(f'{data_path}/bfbc/{dataset}.csv'))) # index => count

def get_chunk_size(_divisors):
  """
  Get the hypothetical average chunk size with the passed set of divisors.
  """
  total_chunk_count = sum(byte_pairs[i] for i in _divisors) + 1
  return (dataset_size) / total_chunk_count + min

print(byte_pairs)
for i in range(len(byte_pairs)):
    if len(divisors) == 0:
        print(get_chunk_size(divisors + [i]))
        if get_chunk_size(divisors + [i]) >= target: 
            divisors.append(i)
    else:
        if abs(target - get_chunk_size(divisors + [i])) < abs(target - get_chunk_size(divisors)):
            divisors.append(i)
       

# print("Divisors: ", len(divisors), divisors)
# print("Dataset size: ", dataset_size)
# cs = get_chunk_size(divisors)
# print(f"Chunk size: {cs} ({(cs - target) / target*100.0:.2f}%)")

print(' '.join(str(i) for i in divisors))
