from string import ascii_lowercase
from string import ascii_uppercase
from itertools import chain

def manhatten_distance(coord, keys, data):
    col, row = coord
    res = data.copy()
    for a, b in keys:
        dist = abs(col - a) + abs(row - b)
        res[(a, b)] = dist
    rv = min(res, key=lambda x: res[x])
    if list(res.values()).count(res[rv]) > 1:
        return False
    return rv

def read_file(filename):
    data = {}
    keys = []
    with open('data/day06.in') as f:
        for line in f:
            col, row = (int(i) for i in line.split(", "))
            data[(col, row)] = 0
            keys.append((col, row))
    return data, keys

def solveA(data, keys):
    coords = dict(zip(data.keys(), chain(ascii_uppercase, ascii_lowercase)))
    for row in range(350):
        for col in range(400):
            coords = manhatten_distance((col, row), keys, data)
            if not coords:
                continue
            if data[coords] == -1:
                continue
            elif row == 0 or col == 0 or row == 349 or col == 399:
                data[coords] = -1
            else:
                data[coords] += 1

    print('Part 1:', max(data.values()))

# Part 2

def manhatten_sum(coord, keys, data):
    col, row = coord
    res = 0
    for a, b in keys:
        res += abs(col - a) + abs(row - b)
    return res

def solveB(keys, data):
    count = 0
    for row in range(350):
        for col in range(400):
            dist = manhatten_sum((col, row), keys, data)
            if dist < 10000:
                count += 1
    print('Part 2:', count)

def main():
    data, keys = read_file('data/day06.in')
    solveA(data, keys)
    solveB(keys, data)


if __name__ == '__main__':
    main()
