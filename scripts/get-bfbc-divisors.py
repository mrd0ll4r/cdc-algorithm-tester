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


dataset = sys.argv[1]
dataset_size = os.stat(f'data/{dataset}').st_size
target = int(sys.argv[2])
min = int(sys.argv[3])
divisors = []
cs = dataset_size # expected average chunk size with current set of divisors
byte_pairs = list(map(lambda x: x['count'], read_csv(f'data/bfbc/{dataset}.csv'))) # index => count

def get_chunk_size(_divisors):
  """
  Get the hypothetical average chunk size with the passed set of divisors.
  """
  total_chunk_count = sum(byte_pairs[i] for i in _divisors) + 1
  return (dataset_size) / total_chunk_count + min


for i in range(len(byte_pairs)):
  if get_chunk_size(divisors + [i]) >= target: 
    divisors.append(i)

lastI = len(byte_pairs) - 1
if lastI not in divisors and get_chunk_size(divisors + [lastI]) >= target: 
    divisors.append(lastI)

#print("Divisors: ", len(divisors), divisors)
#print("Dataset size: ", dataset_size)
#cs = get_chunk_size(divisors)
#print(f"Chunk size: {cs} ({(cs - target) / target*100.0:.2f}%)")

print(' '.join(str(i) for i in divisors))