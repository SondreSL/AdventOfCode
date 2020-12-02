from collections import deque
from itertools import count
import numpy as np
from numba import jit

ELVES = 3012210

@jit(nopython=True)
def find_winner(players):
    ring = np.ones(players + 1)
    ring[0] = 0

    steal = False
    alive_elves = players
    thief = 0

    while True:
        for i in range(len(ring)):
            if steal and ring[i] != 0:
                ring[thief] = ring[thief] + ring[i]
                ring[i] = 0
                alive_elves = alive_elves - 1
                if alive_elves == 1:
                    return thief
                steal = False
            elif not steal and ring[i] != 0:
                thief = i
                steal = True

def solveB(players):
    ring = np.ones(players + 1)
    ring[0] = 0

    steal = False
    alive_elves = players
    thief = False
    found = 0
    target_count = players // 2
    n = 1

    while True:
        for i in (i % players for i in count(n)):
            if thief and ring[i] != 0:
                found += 1
                if found == target_count:
                    ring[thief] += ring[i]
                    ring[i] = 0
                    alive_elves -= 1
                    print(f'Thief: {thief}, victim: {i}, Ring: {ring[1:]}')
                    if alive_elves == 1:
                        return thief
                    n = thief + 1
                    thief = False
                    break
            elif not thief and ring[i] != 0:
                    thief = i
                    target_count = alive_elves // 2
                    found = 0


assert solveB(5) == 2

def main():
    # print('Part 1:', find_winner(ELVES))
    # print('Part 2:', solveB(ELVES))
    pass

if __name__ == "__main__":
    main()

# 1830116 (Too high)
