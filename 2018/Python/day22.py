"""Advent of Code 2018 Day 22
Very much inspired by
https://www.reddit.com/r/adventofcode/comments/a8i1cy/2018_day_22_solutions/ecax3s5
and
https://www.reddit.com/r/adventofcode/comments/a8i1cy/2018_day_22_solutions/ecax2bg
"""
from collections import namedtuple
from heapq import heappop, heappush
from functools import lru_cache

MOD = 20183

# Target input
DEPTH = 4002
TX, TY = 5, 746

def erosion_level(x, y):
    geo_index = g_index(x, y)
    return (geo_index + DEPTH) % MOD

@lru_cache(None)
def g_index(x, y):
    if x == y == 0:
        return 0
    if x == TX and y == TY:
        return 0
    if y == 0:
        return x * 16807
    if x == 0:
        return y * 48271
    return ((g_index(x - 1, y) + DEPTH) *
            (g_index(x, y - 1) + DEPTH) % MOD)

def region(x,y):
    return erosion_level(x,y) % 3

def solveA():
    return sum(region(x,y)
               for x in range(TX + 1)
               for y in range(TY + 1))

def solveB():
    lengths = {}                # x, y, gear: distance
    queue = [(0, 0, 0, 1)]      # distance, x, y, gear

    while queue:
        mins, x, y, gear = heappop(queue)
        if (x, y, gear) in lengths and lengths[(x, y, gear)] <= mins:
            continue
        lengths[(x, y, gear)] = mins
        if (x, y, gear) == (TX, TY, 1):
            return mins
        for i in range(3):
            if i != gear and i != region(x, y):
                heappush(queue, (mins + 7, x, y, i))

        for dx, dy in ((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)):
            if dx < 0 or dy < 0:
                continue
            if dx > TX + 50 or dy > TY + 50:
                continue
            if region(dx, dy) == gear:
                continue
            heappush(queue, (mins + 1, dx, dy, gear))


def main():
    print('Part 1:', solveA())
    print('Part 2:', solveB())

if __name__ == "__main__":
    main()


# Attempts: 1085
