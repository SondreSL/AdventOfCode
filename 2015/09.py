from collections import defaultdict
from collections import deque
from itertools import permutations

def read_file(filename):
    data  = defaultdict(dict)
    with open(filename) as f:
        for line in f:
             a, _, b, _, dist = line.split()
             data[a][b] = int(dist)
             data[b][a] = int(dist)
    return data

def max_distance(data):
    dist = max(sum(data[a][b] for a, b in zip(perm, perm[1:])) for perm in create_perms(data))
    print(f'Part 2: {dist}')
    return dist

def min_distance(data):
    min_dist = min(sum(data[a][b] for a, b in zip(perm, perm[1:])) for perm in create_perms(data))
    print(f'Part 1: {min_dist}')
    return min_dist

def create_perms(data):
    cities = list(data.keys())
    yield from (perm for perm in permutations(cities))

def main():
    cities = read_file('09.in')
    min_distance(cities)
    max_distance(cities)

if __name__ == "__main__":
    main()

# Attemps: 464
#          269 (Too high)
