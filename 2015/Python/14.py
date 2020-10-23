"""Advent of Code 2015 Day 14"""
from collections import defaultdict
from collections import namedtuple
import re

Deer = namedtuple('Deer', ['speed', 'sec', 'rest'])

def read_file(filename):
    lines = open(filename).read().splitlines()
    data = {}
    for line in lines:
        speed, sec, rest = map(int, re.findall(r'\d+', line))
        data[line.split()[0]] = Deer(speed, sec, rest)
    return data

def distance(name, deer, time):
    speed, sec, rest = deer
    dist = 0
    for _ in range(time):
        if sec:
            sec -= 1
            dist += speed
            continue
        if rest:
            rest -= 1
            if rest == 0:
                sec = deer.sec
                rest = deer.rest
    return dist, name

def solveA(data, time):
    return max(distance(r, data[r], time) for r in data.keys())

def solveB(data, time):
    score = defaultdict(int)
    for i in range(time):
        dist, name = solveA(data, i)
        score[name] += 1
    return max(score.values())

def main():
    data = read_file('14.in')
    print('Part 1:', solveA(data, 2503)[0])
    print('Part 2:', solveB(data, 2503))

if __name__ == "__main__":
    main()

# Attempts: 2640
