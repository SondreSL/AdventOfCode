"""Advent of Code 2018 Day 16"""
from ast import literal_eval

FUNCS = [lambda xs, x, y: xs[x] + xs[y],
         lambda xs, x, y: xs[x] + y,
         lambda xs, x, y: xs[x] * xs[y],
         lambda xs, x, y: xs[x] * y,
         lambda xs, x, y: xs[x] & xs[y],
         lambda xs, x, y: xs[x] & y,
         lambda xs, x, y: xs[x] | xs[y],
         lambda xs, x, y: xs[x] | y,
         lambda xs, x, y: xs[x],
         lambda xs, x, y: x,
         lambda xs, x, y: 1 if x > xs[y] else 0,
         lambda xs, x, y: 1 if xs[x] > y else 0,
         lambda xs, x, y: 1 if xs[x] > xs[y] else 0,
         lambda xs, x, y: 1 if x == xs[y] else 0,
         lambda xs, x, y: 1 if xs[x] == y else 0,
         lambda xs, x, y: 1 if xs[x] == xs[y] else 0]

def read_file(filename):
    with open(filename) as f:
        part1 = []
        for line in f:
            if line[0] != 'B':
                break
            sub = []
            sub.append(literal_eval(line[8:]))
            sub.append([int(i) for i in next(f).strip().split()])
            sub.append(literal_eval(next(f).strip()[8:]))
            part1.append(sub)
            next(f)
        part2 = [[int(i) for i in line.split()] for line in f.read().splitlines() if line]
    return part1, part2


def solveA(lines):
    count = 0
    for b, c, a in lines:
        if sum([a == res for res in [operation(b,c,f) for f in FUNCS]]) >= 3:
            count += 1
    return count

def operation(before, code, func):
    before = list(before)
    _, v1, v2, res = code
    before[res] = func(before, v1, v2)
    return before

def find_opcode(lines):
    codes = {i: set(i for i in range(16)) for i in range(16)}
    for b, c, a in lines:
        mapping = [a == res for res in [operation(b,c,f) for f in FUNCS]]
        for i, val in enumerate(mapping):
            if not val:
                codes[c[0]].discard(i)
        if len(codes[c[0]]) == 1:
            for code in codes:
                if code != c[0]:
                    codes[code] -= codes[c[0]]
    codes = {k: FUNCS[v.pop()] for k, v in codes.items()}
    return codes

def solveB(lines, codes):
    reg = [0, 0, 0, 0]
    for code in lines:
        reg = operation(reg, code, codes[code[0]]) 
    return reg

if __name__ == "__main__":
    part1, part2 = read_file('day16.txt')
    # print('Part 1:', solveA(part1))
    codes = find_opcode(part1)
    print(solveB(part2, codes))

