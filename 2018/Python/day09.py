"""Advent of Code 2018 Day 9"""
# Input: 455 players; last marble is worth 71223 points
from collections import deque
from itertools import cycle


def marble_game(players, max_marble):
    cont = deque([0])
    score = {n: 0 for n in range(1, players + 1)}
    players = cycle(score.keys())

    for i in range(1, max_marble + 1):
        player = next(players)
        if i % 23 == 0:
            for _ in range(6):
                cont.appendleft(cont.pop())
            score[player] += cont.pop() + i
        else:
            cont.append(cont.popleft())
            cont.append(cont.popleft())
            cont.appendleft(i)

    return max(score.values())


print("Part 1: ", marble_game(455, 71223))
print("Part 2: ", marble_game(455, 71223 * 100))
