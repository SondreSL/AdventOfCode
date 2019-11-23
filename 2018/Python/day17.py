from collections import namedtuple
from dataclasses import dataclass
import itertools
from collections import deque
from pprint import pprint
import enum

def read_file(filename):
    with open(filename) as f:
        return f.read().splitlines()

class Pos(namedtuple('Pos', ['x', 'y'])):

    def __add__(self, other):
        return type(self)(self.x + other.x, self.y + other.y)

    @property
    def neighbours(self):
        """Only want to go left, right, or down, else we are going 'backwards'."""
        return [self + d for d in [Pos(0, 1), Pos(-1, 0), Pos(1, 0)]]


class Status(enum.Enum):
    Well = enum.auto()
    Clay = enum.auto()
    Wet = enum.auto()
    Water = enum.auto()


@dataclass(frozen=True, eq=True)
class Feature:
    status: Status
    previous: Pos = Pos(500, 0)
    dec_point: bool = False

class Ground(dict):

    def __init__(self, lines):
        super().__init__()
        self[Pos(500, 0)] = Feature(Status.Well)
        for line in lines:
            x, y = (self.parse_input(i) for i in sorted(line.split(', '), key = lambda x: x[0]))
            for x, y in zip(x, y):
                self[Pos(x,y)] = Feature(Status.Clay)
        self._maximums()
        # self.draw()
        self.fill_map()

    def parse_input(self, val):
        if '.' in val:
            l, r = (int(i) for i in val[2:].split('..'))
            return [i for i in range(l, r + 1)]
        return itertools.cycle([int(val[2:])])

    def fill_map(self):
        """Find the number of tiles the water can reach."""
        total_tiles = 0
        water = Pos(500, 1)

        seen = deque([Pos(500, 0)])
        not_water = set()

        while water and water != Pos(500, 0):
            water = self.run_tap(water, seen)

    def run_tap(self, pos, seen, previous=None):
        if pos + Pos(0, -1) in self and self[pos + Pos(0, -1)].status != Status.Clay:
            previous = pos + Pos(0, -1)
        self[pos] = Feature(Status.Wet, previous=previous)

        for nb in pos.neighbours:
            if nb in self:
                continue
            if pos + Pos(0, 1) in self and self[pos + Pos(0, 1)].status == Status.Wet:
                return seen.popleft()
            if self.maxX < nb.x < self.minX or self.maxY < nb.y:
                return seen.popleft()
            if nb != pos + Pos(0, 1) and previous == pos + Pos(0, -1):
                seen.appendleft(pos)
            return nb
        else:
            self.make_water(pos)
            if previous == pos + Pos(0, -1):
                return previous
        new_pos = seen.popleft()
        return new_pos

    def make_water(self, pos):
        w = set()
        w.add(pos)
        l_pos = pos + Pos(-1, 0)
        while l_pos in self:
            if self[l_pos].status == Status.Clay:
                l_pos = self[l_pos].status
                break
            if l_pos + Pos(0, 1) in self and self[l_pos + Pos(0, 1)].status == Status.Wet:
                return
            w.add(l_pos)
            l_pos += Pos(-1, 0)
        r_pos = pos + Pos(1, 0)
        while r_pos in self:
            if self[r_pos].status == Status.Clay:
                r_pos = self[r_pos].status
                break
            if r_pos + Pos(0, 1) in self and self[r_pos + Pos(0, 1)].status == Status.Wet:
                return
            w.add(r_pos)
            r_pos += Pos(1, 0)
        if l_pos == r_pos:
            for i in w:
                self[i] = Feature(Status.Water)

    def count_wet(self):
        return sum(1 for v in self.values() if v.status == Status.Wet)

    def count_water(self):
        return sum(1 for v in self.values() if v.status == Status.Water)

    def solveA(self):
        return self.count_water() + self.count_wet() - 7

    def _maximums(self):
        self.minX = min(x for x, _ in self) - 3
        self.maxX = max(x for x, _ in self) + 3
        self.maxY = max(y for _, y in self)


    def draw(self):
        """Print the entire grid to terminal."""
        signs = {Status.Well: 'x', Status.Clay: '#', Status.Wet: '|', Status.Water: '~'}
        for y in range(self.maxY + 1):
            for x in range(self.minX - 4, self.maxX + 5):
                if (x,y) in self:
                    print(signs[self[(x,y)].status], end='')
                else:
                    print(' ', end='')
            print()
        print()


def main(filename):
    lines = read_file(filename)

    ground = Ground(lines)
    ground.draw()
    print(f'Part 1: {ground.solveA()}')
    print(f'Part 2: {ground.count_water()}')

if __name__ == "__main__":
    # main('test.txt')
    main('day17.txt')


# Attempts:
# 34251
