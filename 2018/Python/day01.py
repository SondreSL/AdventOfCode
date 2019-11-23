# Advent of Code 2018 - Day 1
from itertools import accumulate
from itertools import cycle


# -- Part 1
with open('data/day01.txt') as f:
    print('Part 1:', sum(int(i) for i in f))

# -- Part 2
with open('data/day01.txt') as f:
    nums = [int(i) for i in f]

seen = {0}

for i in accumulate(cycle(nums)):
    if i not in seen:
        seen.add(i)
    else:
        print('Part 2:', i)
        break
