#!/usr/bin/env python3

import sys
from scipy.optimize import fsolve

if len(sys.argv) != 2:
    raise ValueError("Invalid number of arguments. Usage: python find-ram-params.py <target>")

def equation(h, mu):
    sum_term = sum(m * (((m+1)/256)**h - (m/256)**h) for m in range(256))
    return mu - (h + (1 - (1/256) * sum_term) ** -1)

target = float(sys.argv[1])
h_solution = fsolve(equation, 1, args=(target))
print(round(h_solution[0]))