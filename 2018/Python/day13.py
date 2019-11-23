"""Advent of Code Day 13"""
from string import ascii_uppercase
from itertools import cycle

def move(cart, carts, grid):
    direction, coords, old, moves = carts[cart]
    x, y = coords
    grid[y][x] = old
    nX, nY = new_coords(direction, coords, grid)

    new_sign = grid[nY][nX]
    new_dir = new_direction(direction, moves, new_sign)
    new_cart = [new_dir, (nX, nY), new_sign, moves]

    if grid[nY][nX] in ascii_uppercase:
        print(f"Crash! -> ({nX}, {nY})")
        carts.pop(cart)
        old = grid[nY][nX]
        grid[nY][nX] = carts[old][2]
        carts.pop(old)
    else:
        grid[nY][nX] = cart
        carts[cart] = new_cart


def new_direction(direction, moves, new_sign):
    dirs = ['<', '^', '>', 'v']
    i = dirs.index(direction)
    curr_dir = direction
    new_dir = curr_dir
    if new_sign == '+':
        nest_move = next(moves)
        if nest_move == 'left':
            new_dir = dirs[i - 1]
        elif nest_move == 'right':
            new_dir = dirs[(i + 1) % 4]
    elif new_sign == '\\':
        if curr_dir == '<':
            new_dir = '^'
        elif curr_dir == '>':
            new_dir = 'v'
        elif curr_dir == '^':
            new_dir = '<'
        else:
            new_dir = '>'
    elif new_sign == '/':
        if curr_dir == '<':
            new_dir = 'v'
        elif curr_dir == '>':
            new_dir = '^'
        elif curr_dir == '^':
            new_dir = '>'
        else:
            new_dir = '<'
    return new_dir


def new_coords(direction, coords, grid):
    if direction == '<':
        new = coords[0] - 1, coords[1]
    elif direction == '>':
        new = coords[0] + 1, coords[1]
    elif direction == '^':
        new = coords[0], coords[1] - 1
    else:
        new = coords[0], coords[1] + 1
    return new


def sorted_keys(carts):
    return sorted(list(carts), key=lambda x: (carts[x][1][0], carts[x][1][1]))

def main():
    filename = 'day13.txt'

    with open(filename) as f:
        # grid = []
        # for line in f:
        #     grid.append(list(line)[:-1])
        grid = [list(line)[:-1] for line in f]

    letters = iter(ascii_uppercase)
    carts = {}

    for y, row in enumerate(grid):
        for x, c in enumerate(row):
            if c in 'v^<>':
                key = next(letters)
                moves = cycle(['left', 'forward', 'right'])
                if c in 'v^':
                    carts[key] = [c, (x, y), '|', moves]
                if c in '<>':
                    carts[key] = [c, (x, y), '-', moves]
                grid[y][x] = key

    keys = sorted_keys(carts)
    while len(keys) > 1:
        for k in keys:
            if k not in carts:
                continue
            move(k, carts, grid)
        keys = sorted_keys(carts)

    print("Final cart:", carts[keys[0]][1])

if __name__ == "__main__":
    main()
