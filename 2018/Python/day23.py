"""Advent of Code 2018 Day 23"""
"""Part 2 (solveB) very much copied from https://old.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecdqzdg/"""
from queue import PriorityQueue
from collections import namedtuple
from collections import deque
from collections import Counter
import re

def read_file(filename):
    points = {}
    with open(filename) as f:
        for line in f:
            x, y, z, ran = (int(i) for i in re.findall(r'-*\d+', line))
            points[Point(x,y,z)] = ran
    return points

class Point(namedtuple('Point', ['x', 'y', 'z'])):
    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y, self.z + other.z)

    def distance(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y) + abs(self.z - other.z)

def in_range(p, points):
    return [ps for ps in points if p.distance(ps) <= points[p]]

def solveA(points):
    p = max(points, key=lambda x: points[x])
    return len(in_range(p, points))

def solveB(points):
    queue = PriorityQueue()
    for x, y, z in points:
        d = abs(x) + abs(y) + abs(z)
        queue.put((max(0, d - points[(x,y,z)]), 1))
        queue.put((d + points[(x,y,z)] + 1, -1))
    count = 0
    maxCount = 0
    result = 0
    while not queue.empty():
        dist, e = queue.get()
        count += e
        if count > maxCount:
            maxCount = count
            result = dist
    return result

def main():
    points = read_file('input')
    print(f'Part 1: {solveA(points)}')
    print(f'Part 2: {solveB(points)}')

if __name__ == "__main__":
    main()

