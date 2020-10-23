from collections import defaultdict
from itertools import permutations

def read_file(filename):
    data = defaultdict(dict)
    with open(filename) as f:
        for line in f:
            name, _, sign, n, _, _, _, _, _, _, other = line.split()
            sign = {'gain': 1, 'lose': -1}[sign]
            data[name][other[:-1]] = int(n) * sign
    return data

def happiness(data, perm):
    happy = 0
    for i, name in enumerate(perm):
        l = i - 1
        r = i + 1 if i != (len(perm) - 1) else 0
        l, r = perm[l], perm[r]
        happy += data[name][l]
        happy += data[name][r]
    return happy


def best_perm(data):
    return max(happiness(data, p) for p in permutations(data.keys()))

def solveA(data):
    rv = best_perm(data)
    print('Part 1:', rv)
    return rv

def solveB(data):
    for k in list(data.keys()):
        data['Me'][k] = 0
        data[k]['Me'] = 0
    rv = best_perm(data)
    print('Part 2:', rv)
    return rv

def main():
    data = read_file('13.in')
    solveA(data)
    solveB(data)

if __name__ == "__main__":
    main()
