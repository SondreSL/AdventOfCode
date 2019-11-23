# Advent of Code 2018 - Day 3
# www.github.com/SondreSL
import re

GRID = [[0 for i in range(3000)] for j in range(3000)]


def new_read(filename):
    data = {}
    with open(filename) as f:
        for line in f:
            num, row, col, dx, dy = map(int, re.findall(r'\d+', line))
            data[num] = [(row, col), (dx, dy)]
    count_overlaps(data)
    return data


def count_overlaps(data):
    for (col, row), end in data.values():
        for i in range(end[1]):
            for j in range(end[0]):
                GRID[row + i][col + j] += 1


def findUnique(coords):
    (col, row), end = coords
    return not any((GRID[row + i][col + j] != 1) for i in range(end[1]) for j in range(end[0]))


def find_overlapping(data):
    return sum(1 for line in GRID for cell in line if cell > 1)


def solveB(data):
    return [line for line in data if findUnique(data[line])][0]


def main():
    data = new_read('../data/day03.in')
    print('Part 1:', find_overlapping(data))
    print('Part 2:', solveB(data))


if __name__ == "__main__":
    main()
