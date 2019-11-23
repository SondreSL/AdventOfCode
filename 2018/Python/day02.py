# Advent of Code 2018 - Day 2
# www.github.com/SondreSL

# -- Part 1
from collections import Counter

with open('day02.txt') as f:
    twos = 0
    threes = 0
    for line in f:
        count = Counter(line).values()
        if 2 in count:
            twos += 1
        if 3 in count:
            threes += 1
    print('Part 1: ', twos * threes)


# -- Part 2
from itertools import combinations

with open('day02.txt') as f:
    lines = f.read().split()
    for a, b in combinations(lines, 2):
        eqs = [v != h for h, v in zip(a,b)]
        if sum(eqs) == 1:
            i = eqs.index(1)
            print('Part 2: ', a[:i] + a[i + 1:])
            break
