"""Advent of Code 2018 Day 11"""
# Input: 3628
import numpy as np
from numba import jit

PUZZLE_INPUT = 3628
# PUZZLE_INPUT = 42

@jit
def power_level(x, y, grid_serial):
    return ((((x + 10) * (((x + 10) * y) + grid_serial)) // 100) % 10) - 5

# grid = [[power_level(j, i, PUZZLE_INPUT) for j in range(1, 301)] for i in range(1, 301)]

grid = np.zeros((300, 300))
for j in range(1, 301):
    for i in range(1, 301):
        grid[j - 1, i - 1] = power_level(i, j, PUZZLE_INPUT)


# Casual quintuple for loop
@jit
def find_max():
    maximum = 0
    for s in range(1, 301):
        print(s)
        for i in range(len(grid) - (s - 1)):
            for j in range(len(grid[0]) - (s - 1)):
                val = 0
                for y in range(s):
                    for x in range(s):
                        val += grid[i + y][x + j]
                if val >= maximum:
                    maximum = val
                    coord = (j + 1, i + 1)
                    size = s
    return coord, maximum, size

coord, maximum, size = find_max()
print("Max:", maximum)
print("Coordinates:", coord)
print("Size of sub-grid:", size)
