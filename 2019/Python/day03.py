from dataclasses import dataclass

def read_file(filename):
    paths = []
    with open(filename) as f:
        for line in f:
            paths.append(list(line.strip() for line in line.split()))
        return paths

@dataclass
class Point:
    x: int
    y: int
    length: int

    def __hash__(self):
        return hash((self.x, self.y))

    def __eq__(self, other):
        return (self.x, self.y) == (other.x, other.y)

def man_dist(point):
    return abs(point.x) + abs(point.y)

def points_visited(path):
    start = (0, 0, 0)
    visited = set()
    for inst in path:
        d, count = (inst[0], int(inst[1:]))
        start, new = move(start, d, count)
        visited = visited.union(new)
    return visited

def move(pos, direction, count):
    direction = {'U': (0, 1), 'D': (0, -1), 'R': (1, 0), 'L': (-1, 0)}[direction]
    x, y = direction
    rv = set()
    for _ in range(1, count + 1):
        a, b, steps = pos
        pos = (a + x, b + y, steps + 1)
        rv.add(Point(*pos))
    return pos, frozenset(rv)

def compute(data):
    return points_visited(data[0]), points_visited(data[1])

def solveA(path1, path2):
    return min(map(man_dist, path1.intersection(path2)))

def solveB(path1, path2):
    inter = path1.intersection(path2)
    path1 = {i for i in path1 if i in inter}
    path2 = {i for i in path2 if i in inter}
    return min(i.length + j.length for i in path1 for j in path2 if i == j)


def main():
    data = read_file('data/input-2019-3.txt')
    path1, path2 = compute(data)
    print('Part A:', solveA(path1, path2))
    print('Part B:', solveB(path1, path2))

if __name__ == "__main__":
    main()

# Part A: 209
# Part B: 43258
