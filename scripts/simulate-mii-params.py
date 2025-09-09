import math

def mu(w):
    binom = math.comb(256, w)
    result = 1.14 / (binom * (256 ** -w)) + w
    return result

for w in range(20):
  print(w, round(mu(w)))