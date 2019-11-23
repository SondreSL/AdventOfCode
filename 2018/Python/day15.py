"""Advent of Code 2018 Day 15"""
from operator import itemgetter
from collections import deque
from dataclasses import dataclass


def read_file(filename):
    field = set()
    ps = {}
    id = 0
    with open(filename) as f:
        for y, line in enumerate(f):
            for x, cell in enumerate(line):
                if cell == '#':
                    continue
                else:
                    field.add((x, y))
                if cell in 'GE':
                    ps[(x,y)] = cell, 200, id
                    id += 1
    return field, ps, x, y


def sort_players(xs):
    return sorted(xs.keys(), key=lambda x: (x[1], x[0]))


def draw_map(size, field, players):
    for i in range(size[1] + 1):
        pstring = '    '
        for j in range(size[0]):
            if (j, i) in field:
                if (j,i) in players:
                    print(players[(j,i)][0], end='')
                    pstring += str(players[(j,i)]) + ' '
                else:
                    print('.', end='')
                else:
                print('#', end='')
        print(pstring, end='')
        print()
    print()


def play(field, players, units, dmg):
    """Play one turn of the game. Return True when the game is over."""
    ps = sort_players(units)
    turn_over = set()
    count = 0
    for p in ps:
        if p in units:
            if players[p][2] in turn_over:
                continue
            if not 'E' in [i for i, _, _ in players.values()] or not 'G' in [i for i, _, _ in players.values()]:
                return True
            team = units[p][0]
            if can_attack(p, team, players, turn_over, dmg):
                turn_over.add(players[p][2])
                continue
            else:
                direction = find_move(p, team, players, field)
                if not direction:
                    continue
                if direction == 'u':
                    coord = (p[0], p[1] - 1)
                elif direction == 'l':
                    coord = (p[0] - 1, p[1])
                elif direction == 'r':
                    coord = (p[0] + 1, p[1])
                elif direction == 'd':
                    coord = (p[0], p[1] + 1)
                players[coord] = players[p]
                players.pop(p)
                count += 1
            if can_attack(coord, team, players, turn_over, dmg):
                turn_over.add(players[coord][2])
                continue
    return False


def can_attack(p, team, players, turn_over, dmg):
    if team == 'G':
        dmg = 3
    foes = set()
    for i, j in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
        coord = (p[0] + i, p[1] + j)
        if coord in players and players[coord][0] != team:
            foes.add(coord)
    if foes:
        in_range = min(foes, key = lambda x: (players[x][1], x[1], x[0]))
        players[in_range] = players[in_range][0], players[in_range][1] - dmg, players[in_range][2]
        if players[in_range][1] <= 0:
            turn_over.add(players[in_range][2])
            players.pop(in_range)
        return True
    return False

def find_move(p, team, players, field):
    """Breadth first search for next move."""
    queue = deque([])
    seen = set(p)
    orig = p
    potential_squares = set()
    for i, j, k, s in [(0, -1, 'u', 1), (-1, 0, 'l', 1), (1, 0, 'r', 1), (0, 1, 'd', 1)]:
        coord = (p[0] + i, p[1] + j)
        if coord in field and coord not in players:
            queue.append((coord, k, s))
            seen.add(coord)
    while queue:
        p, direction, steps = queue.popleft()
        for i, j in [(0, -1), (-1, 0), (1, 0), (0, 1)]:
            coord = (p[0] + i, p[1] + j)
            if coord in players:
                if players[coord][0] != team:
                    potential_squares.add((p, direction, steps))
                    continue
                else:
                    continue
            if coord in field and coord not in seen:
                queue.append((coord, direction, steps + 1))
                seen.add(coord)
    if potential_squares:
        smallest_step = min((s for _, _, s in potential_squares))
        min_steps = min([i for i in potential_squares if i[2] == smallest_step], key=lambda x: (x[0][1], x[0][0]))
        return min_steps[1]
    return False

def battle(filename, dmg=3):
    field, players, x, y = read_file(filename)

    size = x, y

    r = 0
    elves = len([i for i in players.values() if i[0] == 'E'])

    while True:
        finished = play(field, players, players, dmg)
        if finished:
            break
        r += 1

    total_hp = sum((i for _, i, _ in players.values()))
    if dmg == 3:
        print('HP:', total_hp, 'Rounds:', r)
        print("Part 1:", total_hp * r)
    if elves == len([i for i in players.values() if i[0] == 'E']):
        print('Part 2:', total_hp * r)
        return True
    return False

def main():
    filename = 'day15.txt'
    battle(filename)
    dmg = 3
    fin = False
    while not fin:
        dmg += 1
        fin = battle(filename, dmg=dmg)

if __name__ == "__main__":
    main()
