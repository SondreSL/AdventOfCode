from collections import namedtuple
import re

def read_file(filename):
    with open(filename) as f:
        return f.read().split(', ')

class Point(namedtuple('Point', ['x', 'y'])):
    def __add__(self, other):
        return Point(self.x + other.x, self.y + other.y)

    def dist(self):
        return abs(self.x) + abs(self.y)

next_move = {'N': {'R': 'E', 'L': 'W'},
             'S': {'R': 'W', 'L': 'E'},
             'W': {'R': 'N', 'L': 'S'},
             'E': {'R': 'S', 'L': 'N'}}

move = {'N': (-1, 0),
        'S': (1, 0),
        'W': (0, -1),
        'E': (0, 1)}

def main():
    data = read_file('input/01.in')
    direction = 'N'
    seen = set()
    repeat = []
    pos = Point(0, 0)
    for i in data:
        direction = next_move[direction][i[0]]
        a = int(i[1:])
        while a:
            if pos in seen:
                repeat.append(pos)
            else:
                seen.add(pos)
            pos += Point(*move[direction])
            a -= 1

    print('Part 1:', pos.dist())
    print('Part 2:', repeat[0].dist())

if __name__ == "__main__":
    main()
