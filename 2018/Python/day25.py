from collections import namedtuple
from collections import defaultdict

class Point(namedtuple('Point', ['x','y','z','a'])):

    def dist(self, other):
        return abs(self.x - other.x) + abs(self.y - other.y) + abs(self.z - other.z) + abs(self.a - other.a)

    def const(self, other):
        return self.dist(other) <= 3

def read_file(filename):
    lines = set()
    with open(filename) as f:
        for line in f:
            line = Point(*[int(i) for i in line.strip().split(',')])
            lines.add(line)
    return lines

def count_constellations(data):
    consts = set(data)
    curr = defaultdict(set)
    const_count = 1
    while consts:
        curr[const_count].add(consts.pop())
        find_one(consts, curr[const_count])
        consts -= curr[const_count]
        const_count += 1
    return max(curr)


def find_one(data, const):
    prev = 0
    added = len(const)
    while prev < len(const):
        prev = len(const)
        for con in data:
            if any(con.const(i) for i in const):
                const.add(con)
        data -= const

def solveA(data):
    print(f'Part 1: {count_constellations(data)}')

def main():
    lines = read_file('test')
    lines = read_file('input')
    solveA(lines)

if __name__ == "__main__":
    main()
