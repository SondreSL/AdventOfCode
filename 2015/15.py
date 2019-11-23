from collections import namedtuple
from operator import mul
from functools import reduce
from itertools import combinations_with_replacement, permutations
import re

def get_data():
    a = """Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
    Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
    Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
    Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8""".splitlines()

    # a = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
    # Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3""".splitlines()

    Ingredient = namedtuple('Flavour', ['capacity', 'durability', 'flavour', 'texture', 'cals'])
    data = []

    for line in a:
        cap, dur, flav, text, cals = map(int, re.findall(r'-*\d', line))
        name = line.split()[0]
        data.append(Ingredient(cap, dur, flav, text, cals))

    return data

def create_perms(n):
    for cuts in combinations_with_replacement(range(101), n):
        if sum(cuts) == 100:
            for p in permutations(cuts):
                yield p

def find_ratio(data, cals=False):
    best = 0
    for p in create_perms(len(data)):
        new = list(map(sum, map(list, zip(*[value(i, v) for i, v in zip(data, p)]))))
        if any(i <= 0 for i in new):
            continue
        if cals:
            if new[-1] != 500:
                continue
        s = reduce(mul, new[:-1])
        if s > best:
            best = s
    return best

def value(ingredient, ratio):
    return [ratio * i for i in tuple(ingredient)]

def solveA(data):
    print('Part 1:', find_ratio(data))
    print('Part 2:', find_ratio(data, cals=True))


def main():
    data = get_data()
    solveA(data)

if __name__ == "__main__":
    main()

# Attempts: 883200000
