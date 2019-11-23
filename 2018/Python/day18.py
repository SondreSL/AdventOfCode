from collections import namedtuple
import itertools

class Point(namedtuple('Point', ['x', 'y'])):

    def __add__(self, other):
        return type(self)(self.x + other.x, self.y + other.y)

    @property
    def neighbours(self):
        return [self + d for d in [Point(i, j) for i in range(-1, 2) for j in range(-1, 2) if not (i == 0 and j == 0)]]


def read_file(filename):
    data = {}
    with open(filename) as f:
        lines = f.read().splitlines()
    for y, row in enumerate(lines):
        for x, cell in enumerate(row):
            data[Point(x, y)] = cell
    return data


def draw_yard(data):
    maxX = max(data, key=lambda x: x.x)
    maxY = max(data, key=lambda x: x.y)
    for i in range(maxY.y + 1):
        for j in range(maxX.x + 1):
            print(data[(j, i)], end='')
        print()
    print()


def run(data):
    to_lumber = set()
    to_open = set()
    to_trees = set()
    for k, v in data.items():
        if v == '.' and sum(1 for i in k.neighbours if i in data and data[i] == '|') >= 3:
            to_trees.add(k)
        elif v == '|' and sum(1 for i in k.neighbours if i in data and data[i] == '#') >= 3:
            to_lumber.add(k)
        elif v == '#' and ('#' not in [data[i] for i in k.neighbours if i in data] or '|' not in [data[i] for i in k.neighbours if i in data]):
            to_open.add(k)
    for i in to_lumber:
        data[i] = '#'
    for i in to_open:
        data[i] = '.'
    for i in to_trees:
        data[i] = '|'


def solveA(data):
    trees = sum(1 for i in data if data[i] == '|')
    yards = sum(1 for i in data if data[i] == '#')
    return trees * yards


def main():
    data = read_file('day18.txt')
    rs = {}
    gen = 1_000_000_000
    count = 0
    for i in itertools.count(1):
        run(data)
        # draw_yard(data)
        if i == 10:
            print('Part 1:', solveA(data))
        r = solveA(data)
        if r in rs:
            count += 1
            if count == 10:
                diff = i - rs[r]
                break
        else:
            count = 0
        rs[r] = i
    while i < (gen - diff):
        i += diff
    while i < gen:
        run(data)
        i += 1
    print(f'Part B: {solveA(data)}')


if __name__ == "__main__":
    main()
