from dataclasses import dataclass
from itertools import count
import re

@dataclass
class Disc:
    num: int
    n_pos: int
    pos: int

def read_file(filename):
    discs = []
    with open(filename) as f:
        for line in f:
            num, n_pos, time, pos = map(int, re.findall(r'\d+', line))
            discs.append(Disc(num, n_pos, pos))
    return discs

def simulate(discs):
    for i in count():
        for d in discs:
            if ((d.pos + i + d.num) % d.n_pos) != 0:
                break
        else:
            return i

def main():
    discs = read_file('input/15.in')
    print('Part 1:', simulate(discs))
    discs.append(Disc(len(discs) + 1, 11, 0))
    print('Part 2:', simulate(discs))

if __name__ == "__main__":
    main()
